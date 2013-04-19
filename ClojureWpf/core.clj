(ns clj-wpf.core
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
  `(clj-wpf.core/with-invoke* ~dispatcher-obj (fn [] ~@body)))

(defmacro with-invoke? [dispatcher-obj? & body]
  `(if (clojure.core/instance? DispatcherObject ~dispatcher-obj?)
     (clj-wpf.core/with-invoke ~dispatcher-obj? ~@body)
     (do ~@body)))

(defn with-begin-invoke* [^DispatcherObject dispatcher-obj func]
  (let [dispatcher (.get_Dispatcher dispatcher-obj)]
    (if (.CheckAccess dispatcher)
      (func)
      (.BeginInvoke dispatcher DispatcherPriority/Normal
                    (sys-func [Object] [] (func))))))

(defmacro with-begin-invoke [dispatcher-obj & body]
  `(clj-wpf.core/with-begin-invoke* ~dispatcher-obj (fn [] ~@body)))

(defmacro with-begin-invoke? [dispatcher-obj? & body]
  `(if (clojure.core/instance? DispatcherObject ~dispatcher-obj?)
     (clj-wpf.core/with-begin-invoke ~dispatcher-obj? ~@body)
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

(defn- resolve-element-type
  "Resolves a symbol or a keyword to a XamlType in the specified xaml-ctxt or
   the default-xaml-context.  The namespace part of the symbol or keyword can be
   correspond to a namespace prefix (as specificed in the xaml-ctxt map),
   be left off (to use the default-xaml-context)."
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
          path-res-expr `(clj-wpf.core/find-elem-throw ~target ~path-expr)]
      [target path-res-expr])
    [target-path-expr target-path-expr]))

;;; ## ```doat``` and ```at``` macros

(defmacro doat [target-path & body]
  (let [[dispatcher-obj target] (compile-target-path-expr target-path)]
    `(clj-wpf.core/with-invoke? ~dispatcher-obj
       (clojure.core/binding [clj-wpf.core/*cur* ~target]
                             ~@body))))

(defmacro async-doat [target-path & body]
  (let [[dispatcher-obj target] (compile-target-path-expr target-path)]
    `(clj-wpf.core/with-begin-invoke? ~dispatcher-obj
       (clojure.core/binding [clj-wpf.core/*cur* ~target]
                             ~@body))))

;;; ## Compilation of ```property-event-setter-pairs```

(defn- caml-resolve-element [element-type-name]
  (let [name-part (name element-type-name)
        ns-part (namespace element-type-name)
        enparts (str/split name-part #"#")
        type-name-part (first enparts)
        element-name-part (when (> (count enparts) 1) (second enparts))
        element-type-name (symbol ns-part type-name-part)
        xt (resolve-element-type element-type-name)]
    (when xt
      {:type-name element-type-name
       :element-name element-name-part
       :xaml-type xt})))

(defn- caml-form? [x] (and (list? x) (caml-resolve-element (first x))))

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
    (cond
     (.CanWrite prop-info)
     (fn pset-set-property [target value]
       (let [res (if (fn? value)
                   (value (.GetValue prop-info target nil))
                   value)
             res (convert-from ptype type-converter value)]
         (.SetValue prop-info target res nil)))

     (.IsAssignableFrom ICollection ptype)
     (fn pset-mutate-property [target value]
       (let [cur-value (.GetValue prop-info target nil)]
         (cond
          (fn? value)
          (value cur-value)

          (sequential? value)
          (do
            (.Clear cur-value)
            (doseq [x value]
              (.Add cur-value x)))

          :else
          (throw (ex-info "Don't know how to handle property mutator"
                          {:property prop-info
                           :value value})))))

     :else
     (throw (ex-info "Don't know how to handle read-only property that doesn't implement ICollection"
                     {:property prop-info})))))

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
  ([clr-type property-event-key]
     (pset-resolve-key clr-type (get-xaml-type clr-type) property-event-key))
  ([clr-type xaml-type property-event-key]
      (if (instance? clojure.lang.Named property-event-key)
        (pset-resolve-keyword clr-type xaml-type property-event-key)
        (throw (ArgumentException. (str "Don't know how to handle key " property-event-key))))))

(defn pset-compile-val [value]
  (cond
   (caml-form? value) (caml-compile value)
   (vector? value) `[~@(for [x value]
                       (if (caml-form? x)
                         (caml-compile x)
                         x))]
   :default `~value))

(defn pset-compile-late [property-event-setter-pairs]
  (let [kvp (partition 2 property-event-setter-pairs)
        kvp-compiled (doall
                      (for [[k v] kvp]
                        [k (pset-compile-val v)]))
        tsym (gensym "target")
        csym (gensym "cls")]
    `(fn pset-exec-late [~tsym]
       (binding [clj-wpf.core/*cur* ~tsym]
         (let [~csym (clojure.core/type ~tsym)]
           ~@(for [[k v] kvp-compiled]
               `((clj-wpf.core/pset-resolve-key ~csym ~(keyword k))
                 ~tsym
                 ~v)))))))

(defn pset-compile-early
  ([clr-type property-event-setter-pairs]
     (pset-compile-early type (get-xaml-type type property-event-setter-pairs)))
  ([clr-type xaml-type property-event-setter-pairs]
     (doseq [[k v] (partition 2 property-event-setter-pairs)]
       (lookup-property-or-event clr-type (name k)))
     (pset-compile-late property-event-setter-pairs)))

;; (defn pset-compile-early
;;   ([clr-type property-event-setter-pairs]
;;      (pset-compile type (get-xaml-type type property-event-setter-pairs)))
;;   ([clr-type xaml-type property-event-setter-pairs]
;;      (let [kvp (partition 2 property-event-setter-pairs)
;;            kvp-compiled (doall
;;                          (for [[k v] kvp]
;;                            [(pset-resolve-key clr-type xaml-type k) (pset-compile-val v)]))]
;;        (fn pset-exec-early [target]
;;          (binding [*cur* target]
;;            (doseq [[k v] kvp-compiled]
;;              (k target (v))))))))

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
        child-at-exprs (map (fn [form] (apply at-compile async form))
                            child-forms)]
    `(~(if async 'clj-wpf.core/async-doat 'clj-wpf.core/doat)
      ~target-path
      (~(if clr-type
          (pset-compile-early clr-type target-attrs)
          (pset-compile-late target-attrs))
       clj-wpf.core/*cur*)
      ~@child-at-exprs
      clj-wpf.core/*cur*)))

(defmacro at [target-path & forms]
  (apply at-compile false target-path forms))

(defmacro async-at [target-path & forms]
  (apply at-compile true target-path forms))

;;; ## Compilation of caml forms

(defn caml-children-expr [^XamlType xt ^Type clr-type target-sym children]
  (when (and (sequential? children) (seq children))
    (let [children (vec (for [ch children]
                           (if (caml-form? ch) (caml-compile ch) ch)))
          cp (.get_ContentProperty xt)
          member (.get_UnderlyingMember cp)
          cp-xt (.Type cp)
          is-collection (.IsCollection cp-xt)
          children (if is-collection children (first children))]
      (if (instance? PropertyInfo member)
        `((clj-wpf.core/pset-resolve-key ~clr-type ~(keyword (.Name member)))
          ~target-sym
          ~children)
        (throw (ex-info (str "Unable to find content property for" xt) {}))))))

(defn caml-compile
  ([form] (caml-compile nil form))
  ([ns-ctxt form]
     (let [ns-ctxt (or ns-ctxt *xaml-schema-ctxt* default-xaml-context)]
       (binding [*xaml-schema-ctxt* ns-ctxt]
         (let [element-type-name (first form)
               {:keys [element-name xaml-type]}
               (caml-resolve-element element-type-name)]
           (if xaml-type
             (let [clr-type (.get_UnderlyingType xaml-type)
                   elem-sym (with-meta (gensym "e") {:tag type})
                   ctr (.GetConstructor clr-type Type/EmptyTypes)
                   ctr-sym (symbol (str (.FullName clr-type) "."))
                   forms (if element-name
                           [`(.set_Name ~elem-sym ~element-name)] [])
                   more (rest form)
                   attrs? (first more)
                   pset-expr (when (vector? attrs?)
                               `(~(pset-compile-early clr-type xaml-type attrs?)
                                 ~elem-sym))
                   forms (if pset-expr (conj forms pset-expr) forms)
                   children (if pset-expr (rest more) more)
                   children-expr (when (seq children) (caml-children-expr xaml-type clr-type elem-sym children))
                   forms (if children-expr (conj forms children-expr) forms)]
               (when-not ctr
                 (throw (ex-info (str "Unable to find default constructor for type " clr-type)
                                 {:type ::caml-compile-exception
                                  :clr-type clr-type})))
               `(let [~elem-sym (~ctr-sym)]
                  ~@forms
                  ~elem-sym))
             (throw (ex-info (str "Unable to resolve Xaml type " element-type-name) {}))))))))

(defmacro caml
  [& forms]
  (let [ns-map? (first forms)
        ns-map (when (map? ns-map?) (eval ns-map?))
        ns-ctxt (merge *xaml-schema-ctxt* ns-map)
        forms (if ns-map (rest forms) forms)]
    (caml-compile ns-map forms)))

;;;; Data Binding (observables)

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

;;;; Attached Data

(defprotocol IAttachedData (attach! [this target value]))

(defrecord AttachedData [^DependencyProperty prop]
           IAttachedData
           (attach! [this target value] (with-invoke target (.SetValue target prop value)))
           clojure.lang.IDeref
           (deref [this] (when *cur* (.GetValue *cur* prop)))
           clojure.lang.IFn
           (invoke [this target] (.GetValue target prop)))

(defmethod print-method AttachedData [x writer]
  (.Write writer "#<AttachedData ")
  (print-method (:prop x) writer)
  (.Write writer ">"))

(defmacro defattached [name & opts]
  (let [qname (str *ns* "/" (clojure.core/name name))]
    `(clojure.core/defonce ~name
       (clj-wpf.core/->AttachedData
        (System.Windows.DependencyProperty/RegisterAttached
         ~qname System.Object System.Object
         (clj-wpf.core/caml System.Windows.FrameworkPropertyMetadata
                               [:Inherits true ~@opts]))))))

;; TODO
;; (defmacro get-at [target-path property-key])
;; Handle attached property keys

