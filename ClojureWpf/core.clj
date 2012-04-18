(ns ClojureWpf.core
  (:import [System.Windows.Markup XamlReader]
           [System.Threading Thread ApartmentState ParameterizedThreadStart ThreadStart EventWaitHandle EventResetMode]
           [System.Windows.Threading Dispatcher DispatcherObject DispatcherPriority DispatcherUnhandledExceptionEventHandler]
           [System.Windows Application Window EventManager DependencyProperty FrameworkPropertyMetadata LogicalTreeHelper]
           [System.Windows.Data BindingBase Binding BindingOperations]
           [System.Windows.Input ICommand CommandBinding ExecutedRoutedEventHandler CanExecuteRoutedEventHandler]
           [System.Reflection BindingFlags PropertyInfo MethodInfo EventInfo]
           [System.ComponentModel PropertyDescriptor MemberDescriptor]
           [System.Xaml XamlSchemaContext XamlType]
           [System.Collections ICollection]
           [System.IO File]))

(def ^:dynamic *cur* nil)

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

(defn find-elem [target path] (reduce #(LogicalTreeHelper/FindLogicalNode % (name %2)) target path))

(defn find-elem-warn [target path]
  (or (find-elem target path) (println "Unable to find " path " in " target)))

(defn- compile-target-expr [target]
  (let [path? (vector? target)
        target? (when path? (first target))
        implicit-target? (when path? (keyword? target?))
        dispatcher-obj (if path?
                         (if implicit-target? `ClojureWpf.core/*cur* target?)
                         target)
        path-expr (when path? (if implicit-target? (vec target) (vec (rest target))))
        target (if path?
          `(ClojureWpf.core/find-elem-warn ~dispatcher-obj ~path-expr)
          target)]
    [dispatcher-obj target]))

(defmacro doat [target & body]
  (let [[dispatcher-obj target] (compile-target-expr target)]
    `(ClojureWpf.core/with-invoke ~dispatcher-obj
       (clojure.core/binding [ClojureWpf.core/*cur* ~target]
                             ~@body))))

(defmacro async-doat [target & body]
  (let [[dispatcher-obj target] (compile-target-expr target)]
    `(ClojureWpf.core/with-begin-invoke ~dispatcher-obj
       (clojure.core/binding [ClojureWpf.core/*cur* ~target]
                             ~@body))))

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


(def ^:private xamlClassRegex #"x:Class=\"[\w\.]+\"")

(defn- load-dev-xaml [path]
  (let [xaml (slurp path :econding "UTF8")
        xaml (.Replace xamlClassRegex xaml "")]
    (XamlReader/Parse xaml)))

(def ^:dynamic *dev-mode* false)
 
(defn xaml-view
  ([constructor
    mutator
    dev-xaml-path]
     (fn [] (let [view (if (and *dev-mode* dev-xaml-path (File/Exists dev-xaml-path)) (load-dev-xaml dev-xaml-path) (constructor))]
             (mutator view)
             view))))

(defprotocol IAttachedData (attach [this target value]))

(defrecord AttachedData [^DependencyProperty prop]
  clojure.lang.IDeref
  (deref [this] (when *cur* (.GetValue *cur* prop)))
  IAttachedData
  (attach [this target value] (.SetValue target prop value))
  clojure.lang.IFn
  (invoke [this target] (.GetValue target prop)))

(defn create-attached-data [^DependencyProperty prop] (AttachedData. prop))

(defn- event-dg-helper [target evt-method-info handler]
  (let [dg (if-not (instance? Delegate handler)
                 (gen-delegate (.ParameterType (aget (.GetParameters evt-method-info) 0))
                               [s e] (binding [*cur* target] (handler s e)))
                 handler)]
        (.Invoke evt-method-info target (to-array [dg]))
        dg))

(defn- event-helper [target event-key handler prefix]
  (let [mname (str prefix (name event-key))]
    (if-let [m (.GetMethod (.GetType target) mname)]
      (event-dg-helper target m handler)
      (throw (System.MissingMethodException. (str (.GetType target)) mname)))))

(defn += [target event-key handler] (event-helper target event-key handler "add_"))

(defn -= [target event-key handler] (event-helper target event-key handler "remove_"))

(defn command-binding
  ([^ICommand command exec-fn can-exec-fn]
     (CommandBinding. command
                      (gen-delegate ExecutedRoutedEventHandler [s e] (exec-fn s e))
                      (when can-exec-fn
                        (gen-delegate CanExecuteRoutedEventHandler [s e] (can-exec-fn s e)))))
  ([^ICommand command exec-fn]
     (command-binding command exec-fn nil)))

(defn get-static-field [type fname]
  (when-let [f (.GetField type fname (enum-or BindingFlags/Static BindingFlags/Public))]
      (.GetValue f nil)))

(defn get-static-field-throw [type fname]
  (or (get-static-field type fname) (throw (System.MissingFieldException. (str type) fname))))

(defn find-dep-prop [type key]
  (get-static-field type (str (name key) "Property")))

(defn find-routed-event [type key]
  (get-static-field type (str (name key) "Event")))

(defn bind [target key binding]
  (let [dep-prop (if (instance? DependencyProperty key) key (find-dep-prop target key))]
    (BindingOperations/SetBinding target dep-prop binding)))

(declare caml-compile)

(defn gen-invoke [method-str sym & args]
  (let [method-sym (symbol (str "." method-str))]
    `(~method-sym ~sym ~@args)))

(defn- when-type? [t] (eval `(clojure.core/when (clojure.core/instance? System.Type ~t) ~t)))

(defmacro pset-closure [name type args & body]
  (let [[t v] args
        type (when-type? type)
        t (with-meta t {:tag type})]
    `(fn ~name [~t ~v] ~@body)))

(defn- mutate-prop [target prop-info func]
  (let [val (.GetValue prop-info target nil)
        mutated (func val)]
    (when (.CanWrite prop-info)
      (.SetValue prop-info target mutated nil)
      mutated)))

(defn- set-prop-collection [target prop-info val]
  (if-let [existing (.GetValue prop-info target nil)]
    (if (instance? ICollection existing)
      (do (.Clear existing)
          (doseq [x val] (.Add existing x)))
      (.SetValue prop-info target val nil))))

(defn- pset-property-closure [type prop-info]
  (let [dep-prop (find-dep-prop type (.Name prop-info))]
    (fn pset-property-fn [target val]
      (cond (fn? val) (mutate-prop target prop-info val)
        (sequential? val) (set-prop-collection target prop-info val)
        :default (.SetValue prop-info target val nil)))))

(def ^:dynamic ^:private *pset-early-binding* false)

(defmulti pset-property-handler (fn [type prop-info target value] *pset-early-binding*))

(defn pset-property-expr [^Type type ^PropertyInfo prop-info target-sym val-sym]
  (let [getter-name (.Name (.GetGetMethod prop-info))
        getter-invoke (gen-invoke getter-name target-sym)
        setter-name (when-let [setter (.GetSetMethod prop-info)] (.Name setter))]
    (if setter-name
      (let [res-sym (gensym "res")]
         `(if (clojure.core/fn? ~val-sym)
           (let [~res-sym (~val-sym ~getter-invoke)]
             ~(gen-invoke setter-name target-sym res-sym))
           ~(gen-invoke setter-name target-sym val-sym)))
      (let [res-sym (with-meta (gensym "res") {:tag ICollection})]
        `(if (clojure.core/fn? ~val-sym)
           (~val-sym ~getter-invoke)
           (let [~res-sym ~getter-invoke]
             (.Clear ~res-sym)
             (clojure.core/doseq [x# ~val-sym] (.Add ~res-sym x#))))))))

(defmethod pset-property-handler true [^Type type ^PropertyInfo prop-info target-sym val-sym]
  (pset-property-expr type prop-info target-sym val-sym))

(defmethod pset-property-handler false [^Type type ^PropertyInfo prop-info target value]
  (if (.CanWrite prop-info)
    (if (fn? value)
      (.SetValue prop-info target (value (.GetValue prop-info target nil)) nil)
      (.SetValue prop-info target value nil))
    (if (fn? value)
      (value (.GetValue target nil))
      (let [^ICollection coll (.GetValue target nil)]
        (.Clear coll)
        (doseq [x value] (.Add coll x))))))

(defn- pset-event-closure [type event-info]
  (let [add-method (.GetAddMethod event-info)]
    (fn [target value] (event-dg-helper target add-method value))))

(defn- pset-method-closure [type method-info]
  (fn [target value] (.Invoke method-info target (to-array value))))

(defn- pset-method-expr [type method-info target-sym val-sym]
  
  )

(defn- pset-compile-member-key [type key]
  (let [name (name key)]
        (let [members (.GetMember type name)]
          (if-let [member (first members)]
            (do
              (cond
               (instance? PropertyInfo member) (pset-property-closure type member)
               (instance? EventInfo member) (pset-event-closure type member)
               (instance? MethodInfo member) (pset-method-closure type member)
               :default (throw (InvalidOperationException. (str "Don't know how to handle " member " on " type)))))
            (throw (MissingMemberException. (str type) name))))))

(defn- pset-handle-member-key [^Type type key target val]
  (let [name (name key)]
        (let [members (.GetMember type name)]
          (if-let [member (first members)]
            (do
              (cond
               (instance? PropertyInfo member) (pset-property-handler type member target val)
               :default (throw (InvalidOperationException. (str "Don't know how to handle " member " on " type)))))
            (throw (MissingMemberException. (str type) name))))))

(defn- pset-compile-keyword [type kw]
  (cond 
   (= kw :*cur*) (fn [t v]
                    (cond
                     (fn? v) (v t) ; Invoke val on current target
                     (instance? CommandBinding v) (.Add (.CommandBindings t) v)
                     :default (throw (ArgumentException. (str "Don't know how to apply " v " to " t " in :*cur setter")))))
        :default (pset-compile-member-key type kw)))

(defn- pset-handle-keyword [^Type type kw target val]
  (cond 
        :default (pset-handle-member-key type kw target val)))

(defn pset-compile-key [type key]
  (cond
   (keyword? key) (pset-compile-keyword type key)
   (instance? AttachedData key) (fn [t v] (attach key t v))
   (instance? DependencyProperty key) (throw (NotImplementedException.))
   :default (throw (ArgumentException. (str "Don't know how to handle key " key)))))

(defn pset-handle-key [^Type type key target val]
  (cond
   (keyword? key) (pset-handle-keyword type key target val)
   :default (throw (ArgumentException. (str "Don't know how to handle key " key)))))

(defn- caml-form? [x] (and (list? x) (keyword? (first x))))

(defn- pset-compile-val [val]
  (cond
   (caml-form? val) (caml-compile val)
   (vector? val) (vec (for [x val]
                        (if (caml-form? x) (caml-compile x) x)))
   :default val))

(defn- pset-compile-late [target setters]
  (let [type (.GetType target)]
    (binding [*cur* target]
     (doseq [[key val] (partition 2 setters)]
       ((pset-compile-key type key) target val)))
    target))

(defn- pset-compile* [type target setters]
  (if type
    (let [mutator-vals (for [[key val] (partition 2 setters)]
                         [(pset-compile-key type key) (pset-compile-val val)])]
[]      `(do ~@(for [[m v] mutator-vals] `(~m ~target ~v))))
    (let [key-vals (for [[key val] (partition 2 setters)]
                     [key (pset-compile-val val)])
          tsym (gensym "type")]
      `(let [~tsym (.GetType ~target)]
         ~@(for [[key val] key-vals] `((ClojureWpf.core/pset-compile-key ~tsym ~key) ~target ~val))))))

(defn- pset-compile-setter [^Type type target-sym key val]
  (let [val-sym (gensym "val")]
    `(let [~val-sym ~(pset-compile-val val)]
       ~(binding [*pset-early-binding* true]
          (pset-handle-key type key target-sym val-sym)))))

(defn- pset-compile-setters [^Type type target-sym setters]
  (for [[key val] (partition 2 setters)]
    (pset-compile-setter type target-sym key val)))

(defn- pset-exec-setter [type-sym target-sym key value]
  `(let [val# ~(ClojureWpf.core/pset-compile-val value)]
     (ClojureWpf.core/pset-handle-key ~type-sym ~key ~target-sym val#)))

(defn- pset-exec-setters [type-sym target-sym setters]
  (for [[key val] (partition 2 setters)]
    (pset-exec-setter type-sym target-sym key val)))

(defn- pset-compile [type target setters]
  (let [tsym `ClojureWpf.core/*cur*]
    `(binding [ClojureWpf.core/*cur* ~target] ~(pset-compile* type tsym setters) ClojureWpf.core/*cur*)))

(defn pset-compile-early [^Type type target setters]
  (let [target-sym (with-meta (gensym "t") {:tag type})]
    `(let [~target-sym ~target]
       (binding [ClojureWpf.core/*cur* ~target-sym]
         ~@(pset-compile-setters type target-sym setters)
         ~target-sym))))

(defn pset-compile-late [target setters]
  (let [target-sym (gensym "t")
        type-sym (gensym "type")]
    `(let [~target-sym ~target
           ~type-sym (.GetType ~target-sym)]
       (binding [ClojureWpf.core/*cur* ~target-sym]
         ~@(pset-exec-setters type-sym target-sym setters)
         ~target-sym))))

(defn- pset-compile2 [^Type type target setters]
  (if type
    (pset-compile-early type target setters)
    (pset-compile-late target setters)))

(defmacro pset!* [type target setters]
  (let [type (when-type? type)]
    (pset-compile type target setters)))

(defmacro pset!*2 [type target setters]
  (let [type (when-type? type)]
    (pset-compile2 type target setters)))

(defmacro pset! [& forms]
  (let [type-target? (first forms)
        type (when-type? type-target?) 
        target (if type (second forms) type-target?)
        setters (if type (nnext forms) (next forms))]
    (pset-compile type target setters)))

(defmacro pset!2 [& forms]
  (let [type-target? (first forms)
        type (when-type? type-target?) 
        target (if type (second forms) type-target?)
        setters (if type (nnext forms) (next forms))]
    (pset-compile2 type target setters)))

(defmacro defattached [name & opts]
  (let [qname (str *ns* "/" (clojure.core/name name))]
    `(clojure.core/defonce ~name
       (ClojureWpf.core/create-attached-data
        (System.Windows.DependencyProperty/RegisterAttached
         ~qname System.Object System.Object
         (ClojureWpf.core/pset! (System.Windows.FrameworkPropertyMetadata.)
                                :Inherits true ~@opts))))))

(defattached cur-view)

(defn- split-attrs-forms [forms]
  (let [was-key (atom false)]
    (split-with (fn [x]
                  (cond
                   (not (list? x)) (do (reset! was-key true) true)
                   @was-key (do (reset! was-key false) true)
                   :default false)) forms)))

(declare at-compile)

(defn at-compile [target forms]
  (let [[target-attrs forms] (split-attrs-forms forms)
        tsym (gensym "t")
        xforms (for [form forms]
                 (let [path (first form)
                       setters (rest form)]
                   (at-compile `(ClojureWpf.core/find-elem-warn ~tsym ~path) setters)))
        pset-expr (pset-compile2 nil tsym target-attrs)]
    `(do (let [~tsym ~target] ~pset-expr
              ~@xforms))))

(defmacro at [target & forms]
  (let [[dispatcher-obj target] (compile-target-expr target)
        at-expr (at-compile target forms)]
    `(ClojureWpf.core/with-invoke ~dispatcher-obj
       ~at-expr)))

(defmacro async-at [target & forms]
  (let [[dispatcher-obj target] (compile-target-expr target)
        at-expr (at-compile target forms)]
    `(ClojureWpf.core/with-begin-invoke ~dispatcher-obj
       ~at-expr)))

(def xaml-map
  (apply assoc {}
         (mapcat (fn [xt] [(.get_Name xt) xt])
                 (.GetAllXamlTypes (XamlSchemaContext.) "http://schemas.microsoft.com/winfx/2006/xaml/presentation"))))


(defn caml-children*-expr [invoker elem children]
  `(let [existing# (.GetValue ~invoker ~elem)]
    (if (and existing# (instance? System.Collections.ICollection existing#))
      (doseq [ch# ~children] (.Add existing# ch#))
      (.SetValue ~invoker ~elem ~children))))

(defn caml-children-expr [^XamlType xt ^Type type elem-sym children]
  (when (and (sequential? children) (seq children))
    (let [children* (vec (for [ch children]
                           (if (caml-form? ch) (caml-compile ch) ch)))
          cp (.get_ContentProperty xt)
          member (.get_UnderlyingMember cp)]
      (when (instance? PropertyInfo member)
        (let [val-sym (gensym "val")
              expr (pset-property-expr type member elem-sym val-sym)]
          (if (.CanWrite member)
            `(let [~val-sym (clojure.core/first ~children*)] ~expr)
            `(let [~val-sym ~children*] ~expr)))))))

(defn caml-compile [form]
  (let [nexpr (name (first form))
        enidx (.IndexOf nexpr "#")
        ename (when (> enidx 0) (.Substring nexpr (inc enidx)))
        tname (if ename (.Substring nexpr 0 enidx) nexpr)
        xt (xaml-map tname)]
    (when xt
      (let [type (.get_UnderlyingType xt)
            elem (gensym "e")
            forms (if ename [`(.set_Name ~elem ~ename)] [])
            more (rest form)
            attrs? (first more)
            pset-expr (when (vector? attrs?)
                      (pset-compile2 type elem attrs?))
            forms (if pset-expr (conj forms pset-expr) forms)
            children (if pset-expr (rest more) more)
            children-expr (caml-children-expr xt type elem children)
            forms (if children-expr (conj forms children-expr) forms)]
        `(let [~elem (System.Activator/CreateInstance ~type)]
           ~@forms
           ~elem)))))

(defmacro caml [form]
  (let [compiled (caml-compile form)]
    `~compiled))

(defattached dev-sandbox-refresh)

(defn set-sandbox-refresh [sandbox func]
  (let [window (:window sandbox)]
    (at window
        dev-sandbox-refresh (fn [] (at window :Content (func)))
        :*cur* (fn [wind] (.Execute System.Windows.Input.NavigationCommands/Refresh nil wind)))))

(defn- sandbox-refresh [s e] 
  (binding [*cur* s]
    (when-let [on-refresh @dev-sandbox-refresh]
      (binding [*dev-mode* true] (on-refresh)))))

(defn dev-sandbox [& options]
  (let [sandbox (apply separate-threaded-window options)
        window (:window sandbox)]
    (at window
        :*cur* (command-binding System.Windows.Input.NavigationCommands/Refresh #'sandbox-refresh))
    sandbox))

;; Sample Code

;(defn t3 []  (pset!2 Window (Window.) :Title "Hi"))





