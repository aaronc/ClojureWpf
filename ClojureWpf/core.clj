(ns ClojureWpf.core
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
   [System.IO File]
   [ClojureWpf ObservableMap ObservableVector
    EvalReaderWrapper])
  (:require [clojure.string :as str]))

(def ^:dynamic *cur* nil)

;;;; ## Macros for interacting with the WPF Dispatcher

(defn with-invoke* [^DispatcherObject dispatcher-obj func]
  (let [dispatcher (.get_Dispatcher dispatcher-obj)]
    (if (.CheckAccess dispatcher)
      (func)
      (.Invoke dispatcher DispatcherPriority/Normal
               (sys-func [Object] [] (func))))))

(defmacro with-invoke [dispatcher-obj & body]
  `(ClojureWpf.core/with-invoke* ~dispatcher-obj (fn [] ~@body)))

(defmacro with-invoke? [dispatcher-obj? & body]
  `(if (clojure.core/instance? DispatcherObject ~dispatcher-obj?)
     (ClojureWpf.core/with-invoke ~dispatcher-obj? ~@body)
     (do ~@body)))

(defn with-begin-invoke* [^DispatcherObject dispatcher-obj func]
  (let [dispatcher (.get_Dispatcher dispatcher-obj)]
    (if (.CheckAccess dispatcher)
      (func)
      (.BeginInvoke dispatcher DispatcherPriority/Normal
                    (sys-func [Object] [] (func))))))

(defmacro with-begin-invoke [dispatcher-obj & body]
  `(ClojureWpf.core/with-begin-invoke* ~dispatcher-obj (fn [] ~@body)))

(defmacro with-begin-invoke? [dispatcher-obj? & body]
  `(if (clojure.core/instance? DispatcherObject ~dispatcher-obj?)
     (ClojureWpf.core/with-begin-invoke ~dispatcher-obj? ~@body)
     (do ~@body)))


;;; ## Resolution of ```element-type-name```'s

(def ^:private default-xaml-ns {:ns "http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                                :context (XamlReader/GetWpfSchemaContext)})

(def ^:private default-xaml-ns-x {:ns "http://schemas.microsoft.com/winfx/2006/xaml"
                                  :context (XamlReader/GetWpfSchemaContext)})

(def default-xaml-context
  {nil default-xaml-ns :x default-xaml-ns-x})

(def ^:dynamic *xaml-schema-ctxt* default-xaml-context)

(defn- get-xaml-type
  "Gets the corresponding XamlType for the given CLR type."
  [clr-type]
  (when clr-type
    (.GetXamlType
     (XamlReader/GetWpfSchemaContext)
     clr-type)))

;; (defmethod print-dup XamlType [o w]
;;   (.Write w "#=(ClojureWpf.core/get-xaml-type ")
;;   (.Write w (pr-str (.UnderlyingType o)))
;;   (.Write w ")"))

(defn- resolve-element-type
  "Resolves a symbol or a keyword to a XamlType in the specified xaml-ctxt or
   the default-xaml-context.  The namespace part of the symbol or keyword can be
   either correspond to a namespace prefix (as specificed in the xaml-ctxt map),
   be left off (to use the default-xaml-context), or be the name of a namespace
   in a loaded assembly (writing something like :System/String will work)."
  ([element-type-name]
     (resolve-element-type nil element-type-name))
  ([xaml-ctxt element-type-name]
     (when (instance? clojure.lang.Named element-type-name)
       (let [xaml-ctxt (or xaml-ctxt *xaml-schema-ctxt* default-xaml-context) 
             nsname (namespace element-type-name)
             type-name (name element-type-name)
             ns-ctxt (get xaml-ctxt (keyword nsname))
             xaml-nsname (:ns ns-ctxt)
             ctxt (:context ns-ctxt)
             xaml-name (XamlTypeName. xaml-nsname type-name)]
         (or
          (when ctxt
            (.GetXamlType ctxt xaml-name))
          (when-let [resolved (resolve (symbol nsname type-name))]
            (when (instance? Type resolved)
              (get-xaml-type resolved))))))))

;;; ## Parsing of ```target-path``` expressions

(defn find-elem [target path]
  (reduce #(LogicalTreeHelper/FindLogicalNode % (name %2)) (or target *cur*) path))

(defn find-elem-throw [target path]
  (or (find-elem target path)
      (throw (ex-info "Unable to find " path " in " target
                      {:type ::find-elem-exception
                       :path path
                       :target target}))))

(defn compile-target-path-expr [target-path-expr]
  (if (vector? target-path-expr)
    (let [target? (first target-path-expr)
          target (if (keyword? target?) nil target?)
          path-expr (vec (if target (rest target-path-expr) target-path-expr))
          path-res-expr `(ClojureWpf.core/find-elem-throw ~target ~path-expr)]
      [target path-res-expr])
    [target-path-expr target-path-expr]))

;;; ## ```doat``` and ```at``` macros

(defmacro doat [target-path & body]
  (let [[dispatcher-obj target] (compile-target-path-expr target-path)]
    `(ClojureWpf.core/with-invoke? ~dispatcher-obj
       (clojure.core/binding [ClojureWpf.core/*cur* ~target]
                             ~@body))))

(defmacro async-doat [target-path & body]
  (let [[dispatcher-obj target] (compile-target-path-expr target-path)]
    `(ClojureWpf.core/with-begin-invoke? ~dispatcher-obj
       (clojure.core/binding [ClojureWpf.core/*cur* ~target]
                             ~@body))))

;;; ## Compilation of ```property-event-setter-pairs```

;; This code is an obvious hack, but in the end was deemed the best solution to
;; a difficult problem: that of returing a function Clojure as the result of a
;; macro.  Basically there were three choices: 1) write a separate early binding
;; and late binding version of the code (this was tried and was too problematic
;; to maintain and caused bugs), 2) just have a late binding version (this loses
;; many advantages of the early binding version in terms of performance and
;; static XAML type checking), 3) find a work-around such as this.  For now, we
;; are sticking with option 3 because, although it is somewhat of a hack, it
;; provides the best set of features to the user with the least amount of
;; headache in terms of maintainability.

(defmethod print-dup EvalReaderWrapper [o w]
  (.Write w (.Content o)))

(defn- eval-reader [str]
  (doto (EvalReaderWrapper.) (.set_Content str)))

(defn- caml-form? [x] (and (list? x) (resolve-element-type (first x))))

(declare caml-compile)

(defn- get-static-field [type fname]
  (when-let [f (.GetField type fname (enum-or BindingFlags/Static BindingFlags/Public))]
    (.GetValue f nil)))

(defn- find-dep-prop [type key]
  (get-static-field type (str (name key) "Property")))

(defn- find-routed-event [type key]
  (get-static-field type (str (name key) "Event")))

(defn- convert-from [cls-type type-converter value]
  (when value (if type-converter
                (if (instance? cls-type value)
                  value
                  (let [tc (Activator/CreateInstance type-converter)]
                    (when (instance? TypeConverter tc)
                      (when (.CanConvertFrom tc (type value))
                        (.ConvertFrom tc value)))))
                value)))

(defn- pset-resolve-property-key [type xaml-type ^PropertyInfo prop-info]
  (let [tc (.TypeConverter xaml-type)
        ptype (.PropertyType prop-info)
        type-converter (when tc (.ConverterType tc))]
    (if (.CanWrite prop-info)
      (fn pset-set-property [target value]
        (let [res (if (ifn? value)
                    (value (.GetValue prop-info target nil))
                    value)
              res (convert-from ptype type-converter value)]
          (.SetValue prop-info target res nil)))
      (fn pset-mutate-property [target value]))))

(defn- pset-resolve-event-key [type ^EventInfo event-info]
  (let [add-method (.GetAddMethod event-info)
        dg-type (.ParameterType (aget (.GetParameters add-method) 0))]
    (fn pset-add-event-handler [target handler]
      (let [dg (if (instance? Delegate handler)
                 handler
                 (gen-delegate dg-type
                               [s e] (binding [*cur* target] (handler s e))))]
        (.Invoke add-method target (to-array [dg]))))))

(defn lookup-property-or-event [^Type cls member-name]
  (let [members
        (.GetMember
         cls
         member-name
         (enum-or MemberTypes/Property  MemberTypes/Event)
         (enum-or BindingFlags/Public BindingFlags/Instance))
        n (count members)]
    (case n
      0 (throw (ex-info (str "Unable to find property or event named " member-name " for type " cls)
                        {:type ::lookup-property-or-event-exception
                         :class cls
                         :member-name member-name}))
      1 (first members)
      (throw (ex-info (str "Ambiguous match for property event named " member-name " for type " cls)
                        {:type ::lookup-property-or-event-exception
                         :class cls
                         :member-name member-name
                         :found members})))))

(defn- pset-resolve-member-key [^Type type xaml-type member-name]
  (if-let [member (lookup-property-or-event type member-name)]
    (do
      (cond
       (instance? PropertyInfo member) (pset-resolve-property-key type xaml-type member )
       (instance? EventInfo member) (pset-resolve-event-key type member)
       :default (throw (InvalidOperationException. (str "Don't know how to handle " member " on " type)))))
    (throw (MissingMemberException. (str type) name))))

(defn- pset-resolve-keyword [type xaml-type kw]
  (let [name-part (name kw)
        ns-part (namespace kw)]
    (if ns-part
      () ;;TODO
      (pset-resolve-member-key type xaml-type name-part))))

(defn pset-resolve-key
  ([type property-event-key]
     (pset-resolve-key type (get-xaml-type type) property-event-key))
  ([type xaml-type property-event-key]
      (if (instance? clojure.lang.Named property-event-key)
        (pset-resolve-keyword type xaml-type property-event-key)
        (throw (ArgumentException. (str "Don't know how to handle key " property-event-key))))))

(defn pset-compile-val [val]
  (cond
   (caml-form? val) (let [compiled (caml-compile val)]
                      (fn [] (compiled)))
   (vector? val) (let [compiled-seq
                       (for [x val]
                         (if (caml-form? x)
                           (let [compiled (caml-compile x)]
                             (fn [] (compiled)))
                           (fn [] (eval x))))]
                   (fn [] (vec (for [x compiled-seq] (x)))))
   :default (fn [] (eval val))))

(defn pset-compile-late [property-event-setter-pairs]
  (let [kvp (partition 2 property-event-setter-pairs)
        kvp-compiled (doall
                      (for [[k v] kvp]
                        [k (pset-compile-val v)]))]
    (fn pset-exec-late [target]
      (binding [*cur* target]
        (let [cls (type target)
              xaml-type (get-xaml-type cls)]
          (doseq [[k v] kvp-compiled]
            ((pset-resolve-key cls xaml-type k)
             target
             (v))))))))

(defn pset-compile-early
  ([clr-type property-event-setter-pairs]
     (pset-compile type (get-xaml-type type property-event-setter-pairs)))
  ([clr-type xaml-type property-event-setter-pairs]
     (let [kvp (partition 2 property-event-setter-pairs)
           kvp-compiled (doall
                         (for [[k v] kvp]
                           [(pset-resolve-key clr-type xaml-type k) (pset-compile-val v)]))]
       (fn pset-exec-early [target]
         (binding [*cur* target]
           (doseq [[k v] kvp-compiled]
             (k target (v))))))))

;;; ## Compilation of at forms

(defn- split-attrs-forms [forms]
  (let [was-key (atom false)]
    (split-with (fn [x]
                  (cond
                   @was-key (do (reset! was-key false) true)
                   (list? x) false
                   :default (do (reset! was-key true) true))) forms)))

(defn at-compile [async target-path & forms]
  (let [[target-attrs child-forms] (split-attrs-forms forms)
        tag (:tag (meta target-path))
        clr-type (when tag (eval tag))
        pset-eval-str
        (str
         (if clr-type
           (str "#=(ClojureWpf.core/pset-compile-early " (pr-str clr-type))
           "#=(ClojureWpf.core/pset-compile-late")
         " " (pr-str target-attrs) ")")
        child-at-exprs (map (fn [form] (apply at-compile async form))
                            child-forms)]
    `(~(if async 'ClojureWpf.core/async-doat 'ClojureWpf.core/doat)
      ~target-path
      (~(eval-reader pset-eval-str) ClojureWpf.core/*cur*)
      ~@child-at-exprs
      ClojureWpf.core/*cur*)))

(defmacro at [target-path & forms]
  (apply at-compile false target-path forms))

(defmacro async-at [target-path & forms]
  (apply at-compile true target-path forms))

(defn at-compile [target forms]
   (let [[target-attrs forms] (split-attrs-forms forms)
         xforms (for [form forms]
                  (let [path (first form)
                        setters (rest form)]
                    (recur (find-elem-throw ~tsym ~path) setters)))
         pset-expr (pset-compile tsym target-attrs)]
    (do (let [~tsym ~target] ~pset-expr
              ~@xforms))))

;;; ## Compilation of caml forms

(defn caml-children-expr [^XamlType xt ^Type type children]
  (when (and (sequential? children) (seq children))
    (let [children (vec (for [ch children]
                           (if (caml-form? ch) (caml-compile ch) ch)))
          cp (.get_ContentProperty xt)
          member (.get_UnderlyingMember cp)
          cp-xt (.Type cp)
          is-collection (.IsCollection cp-xt)
          children (if is-collection children (first children))]
      (if (instance? PropertyInfo member)
        (let [handler (pset-resolve-property-key type xt member)]
          (fn caml-set-children [target]
            (handler target children)))
        (throw (ex-info (str "Unable to find content property for" xt) {}))))))

;; (defrecord CamlForm [clr-type ctr element-name pset-expr]
;;   clojure.lang.IFn
;;   (invoke [this]
;;     (let [elem (.Invoke ctr nil)]
;;       (when element-name
;;         (set! (.Name elem) element-name))
;;       ;; (when pset-fn
;;       ;;   (pset-fn elem))
;;       ;; (when children-fn
;;       ;;   (children-fn elem))
;;       elem))) 

;; (defn get-no-arg-ctr [^Type clr-type]
;;   (.GetConstructor clr-type Type/EmptyTypes))

;; (defn read-caml-form [clr-type element-name pset-expr]
;;   (let [ctr (get-no-arg-ctr clr-type)]
;;     (when ctr
;;       (CamlForm. clr-type ctr element-name pset-expr))))

;; (defmethod print-dup CamlForm [o w]
;;   (.Write w "(ClojureWpf.core/read-caml-form ")
;;   (.Write w (str/join
;;              " "
;;              (map
;;               pr-str
;;               (map
;;                (partial get o)
;;                [:clr-type :element-name :pset-expr]))))
;;   (.Write w ")"))

(defn caml-compile
  ([form] (caml-compile nil form))
  ([ns-ctxt form]
     (let [ns-ctxt (or ns-ctxt *xaml-schema-ctxt* default-xaml-context)]
       (binding [*xaml-schema-ctxt* ns-ctxt]
         (let [element-type-name (first form)
               name-part (name element-type-name)
               ns-part (namespace element-type-name)
               enparts (str/split name-part #"#")
               type-name-part (first enparts)
               element-name-part (when (> (count enparts) 1) (second enparts))
               element-type-name (symbol ns-part type-name-part)
               xt (resolve-element-type ns-ctxt element-type-name)]
           (if xt
             (let [clr-type (.get_UnderlyingType xt)
                   ctr (.GetConstructor clr-type Type/EmptyTypes)
                   more (rest form)
                   attrs? (first more)
                   pset-fn (when (vector? attrs?)
                             (pset-compile-early clr-type xt attrs?))
                   children (if pset-fn (rest more) more)
                   children-fn (caml-children-expr xt type children)]
               (fn caml-exec []
                 (let [elem (.Invoke ctr nil)]
                   (when element-name-part
                     (set! (.Name elem) element-name-part))
                   (when pset-fn
                     (pset-fn elem))
                   (when children-fn
                     (children-fn elem))
                   elem)))
             (throw (ex-info (str "Unable to resolve Xaml type " element-type-name) {}))))))))

(defmacro caml
  [& forms]
  (let [ns-map? (first forms)
        ns-map (when (map? ns-map?) (eval ns-map?))
        ns-ctxt (merge *xaml-schema-ctxt* ns-map)
        forms (if ns-map (rest forms) forms)
        eval-str (binding [*print-dup* true]
                   (str
                    "#=(ClojureWpf.core/caml-compile "
                    (pr-str ns-map)
                    " "
                    (pr-str forms)
                    ")"))]
    `((fn [] (~(eval-reader eval-str))))))

;; (defn pr-caml-form [o]
;;   (binding [*print-dup* true]
;;     (str "#=(ClojureWpf.core/read-caml-form "
;;          (str/join
;;           " "
;;           (map
;;            pr-str
;;            (map
;;             (partial get o)
;;             [:clr-type :element-name :pset-expr])))
;;          ")")))

;;;; Data Binding

(defmethod print-method ObservableMap [coll w]
  (.Write w "{")
  (.Write w (str/join ", "
                      (for [[k v] coll]
                        (str (pr-str (keyword k)) " " (pr-str v)))))
  (.Write w "}"))

(defmethod print-method ObservableVector [coll w]
  (.Write w "[")
  (.Write w (str/join " " (map pr-str coll)))
  (.Write w "]"))

(defmulti observable class)

(defmethod observable clojure.lang.IPersistentCollection
  [coll]
  (let [res (ObservableVector.)]
       (doseq [x coll] (conj! res x))
       res))

(defmethod observable clojure.lang.IPersistentMap
  [coll]
  (let [res (ObservableMap.)]
    (doseq [[k v] coll] (assoc! res k v))
    res))
