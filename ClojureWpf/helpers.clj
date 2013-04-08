(ns ClojureWpf.helpers
  (:import
   [System.Windows.Markup XamlReader]
   [System.Threading Thread ApartmentState ParameterizedThreadStart ThreadStart
    EventWaitHandle EventResetMode]
   [System.Windows.Threading Dispatcher DispatcherObject DispatcherPriority
    DispatcherUnhandledExceptionEventHandler]
   [System.Windows Application Window EventManager DependencyObject DependencyProperty
    FrameworkPropertyMetadata LogicalTreeHelper]
   [System.Windows.Data BindingBase Binding BindingOperations]
   [System.Windows.Input CommandBinding ExecutedRoutedEventHandler
    CanExecuteRoutedEventHandler]
   [System.Reflection BindingFlags PropertyInfo MethodInfo EventInfo
    MemberInfo ConstructorInfo MemberTypes]
   [System.ComponentModel PropertyDescriptor MemberDescriptor TypeConverterAttribute TypeConverter]
   [System.Xaml XamlSchemaContext XamlType]
   [System.Xaml.Schema XamlTypeName]
   [System.Collections ICollection]
   [System.IO File])
  (:require [clojure.string :as str])
  (:use [ClojureWpf core]))

(def *dispatcher-exception (atom nil))

(defn default-dispatcher-exception-handler [sender args]
  (let [ex (.get_Exception args)]
    (reset! *dispatcher-exception ex)
    (println "Dispatcher Exception: " ex)
    (.set_Handled args true)))

(defn dispatcher-thread [{:keys [init exception-handler] :or {exception-handler default-dispatcher-exception-handler}}]
  (doto (Thread.
         (gen-delegate ThreadStart []
                       (when exception-handler
                         (.add_UnhandledException Dispatcher/CurrentDispatcher
                                                  (gen-delegate DispatcherUnhandledExceptionEventHandler [s e]
                                                                (exception-handler s e))))
                       (when init (init))
                       (Dispatcher/Run)))
                 (.SetApartmentState ApartmentState/STA)
                 (.Start)))

(defattached dispatcher-thread-property)

(defn separate-dispatcher-window
  [& {:keys [exception-handler title show]
      :or {title "Window"
           show true}
      :as opts}]
  (let [win-promise (promise)
        waitHandle (EventWaitHandle. false EventResetMode/AutoReset)
        init-fn
        (fn []
          (let [win (caml Window [Title title])]
            (deliver win-promise win)
            (when show (.Show win))))
        thread (apply dispatcher-thread (merge {:init init-fn} opts))
        win @win-promise]
    (attach! dispatcher-thread-property win thread)
    win))
