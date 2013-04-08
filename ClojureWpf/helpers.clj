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
        thread (dispatcher-thread (merge {:init init-fn} opts))
        win @win-promise]
    (attach! dispatcher-thread-property win thread)
    win))

;;;; Command Binding Helper

(defn command-binding
  ([command exec-fn can-exec-fn]
     (CommandBinding. command
                      (gen-delegate ExecutedRoutedEventHandler [s e] (exec-fn s e))
                      (if can-exec-fn
                        (gen-delegate CanExecuteRoutedEventHandler [s e] (can-exec-fn s e))
                        (gen-delegate CanExecuteRoutedEventHandler [s e] (set! (.CanExecute e) true)))))
  ([command exec-fn]
     (command-binding command exec-fn nil)))

;;;; Helpers for working with XAML

(def ^:private xamlClassRegex #"x:Class=\"[\w\.]+\"")

(defn load-dev-xaml [path]
  (let [xaml (slurp path :econding "UTF8")
        xaml (.Replace xamlClassRegex xaml "")]
    (XamlReader/Parse xaml)))

(def ^:dynamic *dev-mode* false)

(defn xaml-view
  [constructor dev-xaml-path]
  (if (and *dev-mode* dev-xaml-path (File/Exists dev-xaml-path))
    (load-dev-xaml dev-xaml-path) (constructor)))

;;;; Development Mode Helper

(defn dev-sandbox [view-fn & options]
  (let [win (apply separate-dispatcher-window options)]
    (at win
        CommandBindings
        (fn [bindings]
          (.Add bindings
                (command-binding
                 System.Windows.Input.NavigationCommands/Refresh
                 (fn sandbox-refresh (at win Content (view-fn)))))))
    win))

;;;; Helpers for working with WPF Application classes

(defn app-start [application-class]
  (doto (Thread.
         (gen-delegate ThreadStart [] (.Run (Activator/CreateInstance application-class))))
    (.SetApartmentState ApartmentState/STA)
    (.Start)))

(defn get-app-main-window []
  (when-let [app Application/Current]
    (doat app (.MainWindow *cur*))))

