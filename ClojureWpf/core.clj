(ns ClojureWpf.core
  (:import [System.Windows.Markup XamlReader]
           [System.Threading Thread ApartmentState ParameterizedThreadStart ThreadStart]
           [System.Windows.Threading Dispatcher DispatcherObject DispatcherPriority DispatcherUnhandledExceptionEventHandler]
           [System.Windows Application Window]))


(def *dispatcher-exception (atom nil))

(defn- dispatcher-unhandled-execption [sender args]
  (let [ex (.get_Exception args)]
    (reset! *dispatcher-exception ex)
    (println "Dispatcher Exception: " ex)))

(defn separate-threaded-window
  [& {:keys [exception-handler]}]
  (let [window (atom nil)
        ex-handler (or exception-handler dispatcher-unhandled-execption)
        thread (doto (Thread.
                   (gen-delegate ParameterizedThreadStart [window]
                                 (reset! window (Window.))
                                 (.set_Title @window "Window")
                                 (.Show @window)
                                 (.add_UnhandledException Dispatcher/CurrentDispatcher
                                                          (gen-delegate DispatcherUnhandledExceptionEventHandler [s e]
                                                                        (exception-handler s e)))
                                 (Dispatcher/Run)))
               (.SetApartmentState ApartmentState/STA)
               (.Start window))]
    {:thread thread :window @window}))
    

(defn app-start [application-class]
  (doto (Thread.
         (gen-delegate ThreadStart [] (.Run (Activator/CreateInstance application-class))))
    (.SetApartmentState ApartmentState/STA)
    (.Start)))

(def dev-sandbox-setter (atom nil))

(defn dev-sandbox []
  (let [sandbox (separate-threaded-window)]
    (reset! dev-sandbox-setter
            (fn [func] (.BeginInvoke (.get_Dispatcher (:window sandbox)) DispatcherPriority/Normal 
                                 (gen-delegate System.Action [] (.set_Content (:window sandbox) (func))))))
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