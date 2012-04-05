(ns ClojureWpf.core
  (:import [System.Windows.Markup XamlReader]
           [System.Threading Thread ApartmentState ParameterizedThreadStart ThreadStart EventWaitHandle EventResetMode]
           [System.Windows.Threading Dispatcher DispatcherObject DispatcherPriority DispatcherUnhandledExceptionEventHandler]
           [System.Windows Application Window EventManager DependencyProperty]
           [System.Windows.Data BindingBase Binding BindingOperations]
           [System.Reflection BindingFlags]))


(defn with-invoke* [dispatcher-obj func]
  (let [dispatcher (.get_Dispatcher dispatcher-obj)]
    (if (.CheckAccess dispatcher)
      (func)
      (.Invoke dispatcher DispatcherPriority/Normal 
                    (sys-func [Object] [] (func))))))

(defmacro with-invoke [dispatcher-obj & body]
  `(ClojureWpf.core/with-invoke* ~dispatcher-obj (fn [] ~@body)))

(defn with-begin-invoke* [dispatcher-obj func]
  (let [dispatcher (.get_Dispatcher dispatcher-obj)]
    (if (.CheckAccess dispatcher)
      (func)
      (.BeginInvoke dispatcher DispatcherPriority/Normal 
                    (sys-func [Object] [] (func))))))

(defmacro with-begin-invoke [dispatcher-obj & body]
  `(ClojureWpf.core/with-begin-invoke* ~dispatcher-obj (fn [] ~@body)))

(def *dispatcher-exception (atom nil))

(defn- dispatcher-unhandled-exception [sender args]
  (let [ex (.get_Exception args)]
    (reset! *dispatcher-exception ex)
    (println "Dispatcher Exception: " ex)
    (.set_Handled args true)))

(defn separate-threaded-window
  [& {:keys [exception-handler]}]
  (let [window (atom nil)
        ex-handler (or exception-handler dispatcher-unhandled-exception)
        waitHandle (EventWaitHandle. false EventResetMode/AutoReset)
        thread (doto (Thread.
                   (gen-delegate ParameterizedThreadStart [window]
                                 (reset! window (Window.))
                                 (.set_Title @window "Window")
                                 (.Show @window)
                                 (.add_UnhandledException Dispatcher/CurrentDispatcher
                                                          (gen-delegate DispatcherUnhandledExceptionEventHandler [s e]
                                                                        (ex-handler s e)))
                                 (.Set waitHandle)
                                 (Dispatcher/Run)))
               (.SetApartmentState ApartmentState/STA)
               (.Start window))]
    (.WaitOne waitHandle)
    {:thread thread :window @window}))
    

(defn app-start [application-class]
  (doto (Thread.
         (gen-delegate ThreadStart [] (.Run (Activator/CreateInstance application-class))))
    (.SetApartmentState ApartmentState/STA)
    (.Start)))

(def dev-sandbox-setter (atom nil))

(defn dev-sandbox []
  (let [sandbox (separate-threaded-window)
        window (:window sandbox)]
    (reset! dev-sandbox-setter
            (fn [func] (with-invoke window (.set_Content window (func)))))
    sandbox))

(defn sandbox-load [func]
  (when @dev-sandbox-setter
    (@dev-sandbox-setter (fn [] (func :dev true)))))

(defn set-live-reload! [enable])

(defn- load-dev-xaml [path]
  (let [xaml (slurp path :econding "UTF8")]
    (XamlReader/Parse xaml)))

(comment (defn exec-spec [uispec {:keys [dev]}]
           (let [dev-path (:dev-path uispec)
                 ctr (:constructor uispec)
                 elem (if (and dev dev-path) (load-dev-xaml dev-path) ctr)]
             ((:mutator uispec) elem)
             elem))

         (defrecord UISpec [constructor mutator dev-path]
           clojure.lang.IFn
           (invoke [this opts] :a)
           (invoke [this] :b)))

(defn make-ui-spec
  ([constructor
    mutator
    dev-path]
     (let [func (fn [& {:keys [dev]}]
                  (let [elem (if (and dev dev-path) (load-dev-xaml dev-path) (constructor))]
                    (mutator elem)
                    elem))]
       (sandbox-load func)
       func)))

(defmacro defui
  [name
   elem-constructor
   elem-mutator &
   [xaml-dev-path & opts]]
  `(def ~name (ClojureWpf.core/make-ui-spec ~elem-constructor ~elem-mutator ~xaml-dev-path)))

(defn- event-helper [target event-key handler prefix]
  (let [mname (str prefix (name event-key))]
    (if-let [m (.GetMethod (.GetType target) mname)]
      (let [dg (if-not (instance? Delegate handler)
                 (gen-delegate (.ParameterType (aget (.GetParameters m) 0))
                               [s e] (handler s e))
                 handler)]
        (.Invoke m target (to-array [dg]))
        dg)
      (throw (System.MissingMethodException. (str (.GetType target)) mname)))))

(defn += [target event-key handler] (event-helper target event-key handler "add_"))

(defn -= [target event-key handler] (event-helper target event-key handler "remove_"))

(defn find-static-field [target fname]
  (if-let [f (.GetFields (.GetType target) fname (enum-or BindingFlags/Static BindingFlags/Public))]
      (.GetValue f)
      (throw (System.MissingFieldException. (str (.GetType target)) fname))))

(defn find-dep-prop [target key]
  (find-static-field target (str (name key) "Property")))

(defn find-routed-event [target key]
  (find-static-field target (str (name key) "Event")))

(defn bind [target key binding]
  (let [dep-prop (if (instance? DependencyProperty key) key (find-dep-prop target key))]
    (BindingOperations/SetBinding target dep-prop binding)))

(defn set-property-by-key [target key val]
  (if (instance? DependencyProperty key)
    (.SetValue target key val)
    (let [mname (str "set_" (name key))]
      (if-let [m (.GetMethod (.GetType target) mname)]
        (.Invoke m target (to-array [val]))
        (throw (System.MissingMethodException. (str (.GetType target)) mname))))))

(defn set-event-by-key [target key val] (+= target key val))

(defn pset! [target & setters]
  (when target
    (with-invoke target
      (doseq [[key val] (partition 2 setters)]
        (cond
         (fn? val) (set-event-by-key target key val)
         (instance? BindingBase val) (bind target key val)
         :default (set-property-by-key target key val))))))

(defn find-elem [target path]
  (if (empty? path)
    target
    (let [name (name (first path))]
      (.FindName target name))))

(defn find-elem-warn [target path]
  (if-let [elem (find-elem target path)]
    elem
    (println "Unable to find " path " in " target)))

(defmacro at [target & forms]
  (let [xforms (for [form forms]
                 (let [path (first form)
                       setters (rest form)]
                   `(ClojureWpf.core/pset! (ClojureWpf.core/find-elem-warn ~target ~path) ~@setters)))]
    `(ClojureWpf.core/with-invoke ~target
       ~@xforms)))
