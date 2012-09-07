(ns ClojureWpf.data-binding
  (:import [System.Windows.Markup XamlReader]
           [System.Threading Thread ApartmentState ParameterizedThreadStart ThreadStart EventWaitHandle EventResetMode]
           [System.Windows.Threading Dispatcher DispatcherObject DispatcherPriority DispatcherUnhandledExceptionEventHandler]
           [System.Windows Application Window EventManager DependencyObject DependencyProperty FrameworkPropertyMetadata LogicalTreeHelper]
           [System.Windows.Data BindingBase Binding BindingOperations]
           [System.Windows.Input ICommand CommandBinding ExecutedRoutedEventHandler CanExecuteRoutedEventHandler]
           [System.Reflection BindingFlags PropertyInfo MethodInfo EventInfo]
           [System.ComponentModel PropertyDescriptor MemberDescriptor TypeConverterAttribute TypeConverter]
           [System.Xaml XamlSchemaContext XamlType]
           [System.Xaml.Schema XamlTypeName]
           [System.Collections ICollection]
           [System.IO File]
           [ClojureWpf AbstractObservableDictionary AbstractObservableList]))

(defn target-get [target path key]
  (let [sub-path (conj path (keyword key))]
    (wrap-value (get-in @target sub-path) sub-path)))

(declare wrap-value)

(defn dictionary-proxy [target path]
  (let [wrapper
        (proxy [AbstractObservableDictionary] []
          (SetValue [key val]
            (swap! target assoc-in (conj path (keyword key)) val))
          (GetValue [key] (target-get target path key))
          (get_Count [] (count @target))
          (GetEnumerator [] (.GetEnumerator @target))
          (ContainsKey [key] (if (target-get target path key) true false))
          (Clear [] (if (empty? path)
                      (reset! target {})
                      (swap! target assoc-in path {})))
          (Remove [key] (swap! target assoc-in (conj path (keyword key)) nil))
          (get_Keys [] (map name (keys (get-in @target path))))
          (get_Values [] (for [[k v] (get-in @target path)]
                           (wrap-value v (conj path k)))))]
    wrapper))

(defn list-proxy [target path])

(defn wrap-value [val path]
  (cond
   (map? val) (dictionary-proxy val path)
   (sequential? val) (list-proxy val path)
   :default val))
