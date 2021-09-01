(ns cashmere.core-graal
  (:require [cashmere.instance :as ci]
            [cashmere.template :as gt]
            [cashmere.util :as cu]
            [criterium.core :as cc]
            [taoensso.timbre :as log]
            [io.aviso.exception]
            [clojure.data]
            [clojure.inspector]
            [clojure.java.io]
            [clojure.string]
            [clojure.core.async :as async :refer [alt! chan go go-loop put! sliding-buffer <!! <! >! >!!]])
  (:import (org.graalvm.polyglot Context Context$Builder PolyglotException Source Value)
           (org.graalvm.polyglot.proxy ProxyArray ProxyExecutable ProxyObject)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def language-id "js")
(def ^:dynamic *context* nil)
;; stream of 0-arity fns to execute on main thread
(def ^:dynamic *callback-chan* (chan))

;; From taoensso/timbre
(defn ^:private convert-to-clojure
  [class-name method-name]
  (let [[namespace-name & raw-function-ids] (clojure.string/split class-name #"\$")
        ;; Clojure adds __1234 unique ids to the ends of things, remove those.
        function-ids (map #(clojure.string/replace % #"__\d+" "") raw-function-ids)
        ;; In a degenerate case, a protocol method could be called "invoke" or "doInvoke"; we're ignoring
        ;; that possibility here and assuming it's the IFn.invoke(), doInvoke() or
        ;; the invokeStatic method introduced with direct linking in Clojure 1.8.
        all-ids      (if (#{"invoke" "doInvoke" "invokeStatic" "invokePrim"} method-name)
                       function-ids
                       (-> function-ids vec (conj method-name)))]
    ;; The assumption is that no real namespace or function name will contain underscores (the underscores
    ;; are name-mangled dashes).
    (->>
      (cons namespace-name all-ids)
      (map io.aviso.exception/demangle))))

;; From io.aviso.exception
(defn ^:private strip-prefix
  [^String prefix ^String input]
  (let [prefix-len (.length prefix)]
    ;; clojure.string/starts-with? not available in Clojure 1.7.0, so:
    (if (and (.startsWith input prefix)
             (< prefix-len (.length input)))
      (subs input prefix-len)
      input)))

;; From io.aviso.exception
(defn ^:private extension
  [^String file-name]
  (let [x (.lastIndexOf file-name ".")]
    (when (<= 0 x)
      (subs file-name (inc x)))))

;; From io.aviso.exception
(def ^:private clojure-extensions
  #{"clj" "cljc"})

;; From io.aviso.exception
(defn ^:private expand-stack-trace-element
  ;[file-name-prefix ^PolyglotException$StackFrame element]
  [file-name-prefix element]
  (let [host-element  (.toHostFrame element)
        class-name  (.getClassName host-element)
        method-name (.getMethodName host-element)
        dotx        (.lastIndexOf class-name ".")
        #_#_source-section (.getSourceLocation element)
        #_#_source      (when source-section (.getSource source-section))
        #_#_file-name   (or (when source (.getName source)) "")
        file-name   (or (.getFileName host-element) "")
        is-clojure? (->> file-name extension (contains? clojure-extensions))
        names       (if is-clojure? (convert-to-clojure class-name method-name) [])
        name        (clojure.string/join "/" names)
        ; This pattern comes from somewhere inside nREPL, I believe
        [file line] (if (re-matches #"form-init\d+\.clj" file-name)
                      ["REPL Input"]
                      [(strip-prefix file-name-prefix file-name)
                       (-> host-element .getLineNumber)])]
    ;(log/info "file-name" file-name "host-file-name" (or (.getFileName host-element) ""))
    {:file         file
     ; line will sometimes be nil
     :line         (if (and line
                            (pos? line))
                     line)
     :class        class-name
     :package      (if (pos? dotx) (.substring class-name 0 dotx))
     :is-clojure?  is-clojure?
     :simple-class (if (pos? dotx)
                     (.substring class-name (inc dotx))
                     class-name)
     :method       method-name
     ;; Used to calculate column width
     :name         name
     ;; Used to present compound Clojure name with last term highlighted
     :names        names}))

;; From io.aviso.exception
(def ^:private empty-stack-trace-warning
  "Stack trace of root exception is empty; this is likely due to a JVM optimization that can be disabled with -XX:-OmitStackTraceInFastThrow.")

;; From io.aviso.exception
(def ^:private current-dir-prefix
  "Convert the current directory (via property 'user.dir') into a prefix to be omitted from file names."
  (delay (str (System/getProperty "user.dir") "/")))

(defn expand-polygot-stack-trace
  [^PolyglotException polyglot-exception]
  (let [elements (map (partial expand-stack-trace-element @current-dir-prefix) (.getPolyglotStackTrace polyglot-exception))]
    (when (empty? elements)
      (binding [*out* *err*]
        (println empty-stack-trace-warning)
        (flush)))
    elements))

(declare clj->js)
(declare js->clj)

;; Empty object for context locking
;; Locking on the Graal context itself did not work
;; TODO findout why
(def ctx-lock {})

;; Execute a js function
(defn- execute
  [^Value execable & args]
  (locking ctx-lock
    (try
      (log/trace "===== Executing Javascript Function ====")
      (log/trace "fn" execable #_#_"with" args)
      (let [result (.execute execable (object-array (map clj->js args)))]
        (js->clj result))
      (catch PolyglotException pe
        (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
          (log/error pe)))
      (catch Throwable t
        (log/error t)))))

;; From https://gist.github.com/taylorwood/bb3ebfec5d5de3cccc867a9eba216c18
(defmacro ^:private reify-ifn
  "Convenience macro for reifying IFn for executable polyglot Values."
  [^Value v]
  (let [invoke-arity
        (fn [n]
          (let [args (map #(symbol (str "arg" (inc %))) (range n))]
            (if (seq args)
              `(~'invoke [this# ~@args]
                 (execute ~v ~@args))
              `(~'invoke [this#] (execute ~v)))))]
    `(reify
       clojure.lang.IFn
       ~@(map invoke-arity (range 22))
       (~'applyTo [this# args#] (apply execute ~v args#)))))

(require '[clojure.reflect :as r])
(use '[clojure.pprint :only [print-table]])


(defn meta-required?
  [^Value v]
  (when-let [mo (.getMetaObject v)]
    #_(log/info "mo" mo (type mo))
    (when (.hasMembers mo)
      #_(log/trace "mo has members" (.getMemberKeys mo))
      (when (.hasMember mo "type")
        (when-let [t (.getMember mo "type")]
          (let [type-string (js->clj t)]
            #_(log/trace "meta-type" type-string "for" v (type t))
            #_(log/trace (contains? #{"symbol"} type-string))
            #_(log/trace (= "symbol" type-string))
            (contains? #{"symbol"} type-string)))))))

(defn symbol-value?
  [^Value v]
  (when-let [mo (.getMetaObject v)]
    (when-let[mtn (.getMetaQualifiedName mo)]
      (= mtn "symbol"))))

;  Returns a Clojure (or Java) value for given polyglot Value if possible,
;     otherwise throws.
(defmulti js->clj (fn [^Value v]
  #_(log/info "js->clj" v (type v)
  "\n.isHostObject" (.isHostObject v)
  "\n.canExecute" (.canExecute v)
  "\n.canInstantiate" (.canInstantiate v)
  ;"\nmeta-required?" (meta-required? v)
  "\n.hasArrayElement" (.hasArrayElements v)
    "\n.hasMembers" (.hasMembers v)
    "\n.isProxyObject" (.isProxyObject v)
    "\n.isMetaObject" (.isMetaObject v))
  (let [dispatch (cond
                   (nil? v) :nil
                   (.isNull v) :nil
                   (.isHostObject v) :host-object
                   (.isBoolean v) :boolean
                   (.isString v) :string
                   (.isNumber v) :number
                   (.canExecute v) :fn
                   (.canInstantiate v) (assert false "canInstantiate")
                   (.isException v) (assert false "isException")
                   (.isTimeZone v) (assert false "isTimeZone")
                   (.isDuration v) (assert false "isDuration")
                   (.isNativePointer v) (assert false "isNativePointer")
                   (.isDate v) (assert false "isDate")
                   (.isTime v) (assert false "isTime")
                   (symbol-value? v) :symbol
                   (meta-required? v) :meta
                   (.hasArrayElements v) :array
                   (.hasMembers v) (if (.hasMember v "__props__") :props :object)
                   (.isProxyObject v) :proxy
                   :else (do
                           #_(print-table (:members (r/reflect v)))
                           (throw (Exception. (str "Unsupported value v:" v " type:" (class v) " mo:" (.getMetaObject v) " mo-type:" (class (.getMetaObject v)))))
                           #_(log/error "Unsupported value v:" v " type:" (class v) " mo:" (.getMetaObject v) " mo-type:" (class (.getMetaObject v)))
                           #_:string))]
    #_(log/trace "dispatching to" dispatch)
    dispatch)))

(defmethod js->clj :nil
  [^Value v]
  nil)

(defmethod js->clj :host-object
  [^Value v]
  ;(log/trace "host-object" (.asHostObject v) (type (.asHostObject v)))
  (.asHostObject v))

(defmethod js->clj :boolean
  [^Value v]
  (.asBoolean v))

(defmethod js->clj :string
  [^Value v]
  (let [s (.asString v)]
    (if (clojure.string/starts-with? s ":")
      (keyword (subs s 1))
      s)))

(defmethod js->clj :number
  [^Value v]
  (.as v Number))

(defmethod js->clj :fn
  [^Value v]
  (reify-ifn v))

(defmethod js->clj :symbol
  [^Value v]
  (keyword (str v)))

(defmethod js->clj :meta
  [^Value v]
  (let [mo (.getMetaObject v)]
    (log/info "js->clj mo" mo)
    #_(when (.hasMembers v)
      (doseq [k (.getMemberKeys v)]
        (log/trace k (.getMember v k))))
    ; Turn meta objects into clj
    (js->clj mo)))

(defmethod js->clj :array
  [^Value v]
  (into []
        (for [i (range (.getArraySize v))]
          (js->clj (.getArrayElement v i)))))

(defmethod js->clj :props
  [^Value v]
  #_(log/info "Got props" v (type v))
  (let [internal-props (js->clj (.getMember v "__props__"))
        react-props (into {}
                       (for [k (.getMemberKeys v)
                             :when (not (= k "__props__"))
                             :let [prop-v (.getMember v k)]]
                         (let [k (get {"children" :children} k k)]
                           #_(log/info "map value" k prop-v (type prop-v))  
                           [k (js->clj prop-v)])))
        props (merge internal-props
                     react-props)]
    #_(log/info "internal-props" internal-props)
    #_(log/info "react-props children " (get react-props :children))
    #_(log/info "Resulting props children " (get props :children))
    props))

(def key-blocklist
  #{"return"
    "child"
    ; one of these
    ;"elementType"
    ;"type"
    ;"stateNode"
    ;"tag"
    ;"key"
    "current"
    ;"_owner"
    ;"_debugOwner"
    "firstEffect"
    "nextEffect"
    "lastEffect"
    "lane"
    "alternate"
    "next"
    "pendingProps"
    "memoizedProps"
    })

(defn fiber?
  [ks]
  (= #{"tag" "key" "elementType" "type" "stateNode" "return" "child" "sibling" "index" "ref" "pendingProps" "memoizedProps" "updateQueue" "memoizedState" "dependencies" "mode" "effectTag" "nextEffect" "firstEffect" "lastEffect" "expirationTime" "childExpirationTime" "alternate" "actualDuration" "actualStartTime" "selfBaseDuration" "treeBaseDuration" "_debugID" "_debugIsCurrentlyTiming" "_debugSource" "_debugOwner" "_debugNeedsRemount" "_debugHookTypes"} (set ks)))

(defmethod js->clj :object
  [^Value v]
  #_(log/trace "Got object" v (type v))
  #_(log/info "Got object ks" (.getMemberKeys v))
  (let [ks (.getMemberKeys v)
        ks (if (fiber? ks)
             #{"elementType" "type"}
             ks)]
    (into {}
      (for [k ks]
        (do
        #_(log/info "k" k)
        ; Don't recurse up to the parent
        [k
         (if (contains? key-blocklist k)
           nil
           (js->clj (.getMember v k)))])))))

(defmethod js->clj :proxy
  [^Value v]
  (.asProxyObject v))

(defmulti clj->js (fn [x] 
  (cond
    (nil? x) :nil
    (fn? x) :fn
    ; records should be host objects
    (record? x) :default
    (map? x) :map
    (seq? x) :seq
    (set? x) :seq
    (keyword? x) :keyword
    :else :default)))
(defmethod clj->js :nil [x] (Value/asValue x))
(defmethod clj->js :fn [x]
  "Returns a ProxyExecutable instance for given function, allowing it to be
     invoked from polyglot contexts."
  (Value/asValue 
    (reify
      Object
      (toString [this] (str "ProxyExecutable:" x))
      ProxyExecutable
      (execute [_this args]
        #_(log/info "===== Executing Clojure Function ====")
        #_(log/info "fn" x)
        (try
          (let [clj-args (mapv js->clj args)]
            #_(log/info "clj arg " clj-args)
            (clj->js (apply x clj-args)))
          (catch PolyglotException pe
            (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
              (log/error pe))
            (throw pe))
          (catch Throwable t
            (log/error t)
            (throw t)))))))

(defn clj-map->js
  [m]
  (Value/asValue
    (reify ProxyObject
      ; Object  getMember(String key)
      ; Returns the value of the member.
      (getMember [_this k] (clj->js (get m k)))
      ; Object  getMemberKeys()
      ; Returns array of member keys.
      (getMemberKeys [_this] (into-array String (map str (keys m))))
      ; boolean   hasMember(String key)
      ; Returns true if the proxy object contains a member with the given key, or else false.
      (hasMember [_this k] (contains? m k))
      ; void  putMember(String key, Value value)
      ; Sets the value associated with a member.
      (putMember [_this k v])
      ; default boolean   removeMember(String key)
      ; Removes a member key and its value.
      (removeMember [_this k]))))

(defmethod clj->js :map [x]
  #_(log/trace "map object" x)
  (Value/asValue
    (apply hash-map
      (mapcat (fn [[k v]]
        [k #_(clj->js k)
         (clj->js v)])
        x))))

(defn clj-seq->js
  [s]
  (let [v (vec s)]
    (Value/asValue
      (reify ProxyArray
        ;get(long index)
        ;Returns the element at the given index.
        (get [_this index] (clj->js (nth v index)))
        ;long  getSize()
        (getSize [_this] (count v))
        ;Returns the reported size of the array.
        ;default boolean   remove(long index)
        (remove [_this index] false)
        ;Removes the element at the given index.
        ;void  set(long index, Value value)
        (set [_this index value])))))

(defmethod clj->js :seq [x]
  (let [v (vec x)]
  (Value/asValue
    ;(ProxyArray/fromList
      ;(java.util.ArrayList.
        (mapv clj->js v))))

(def ^:dynamic *create-symbol* nil)
(defmethod clj->js :keyword [x]
  (log/info "create symbol" *create-symbol*)
  (if *create-symbol*
    (*create-symbol* (name x))
    (Value/asValue (str ":" (name x)))))

(defmethod clj->js :default [x]
  (Value/asValue x))

(defn props->js
  [props]
  #_(log/trace "props->js" props (type props))
  (if (instance? Value props)
    props
    (Value/asValue
      (ProxyObject/fromMap
        (apply hash-map
          "__props__" (Value/asValue props)
          "key" (get props :key)
          (if (contains? props :ref)
            (let [original (-> props :ref meta :original)]
              ["ref" original])
            []))))))

(defn component->js
  [component]
  #_(log/trace "component->js" component)
  (let [m {"displayName" (or (some-> component meta :display-name str) "Unknown")}]
    (Value/asValue 
      (if (fn? component)
        (reify
          ProxyExecutable
          (execute [_this args]
            (log/trace "===== Executing Clojure Component Function ====")
            (log/trace "fn" component)
            (try
              (let [[props context] (mapv js->clj args)
                    ; unwrap cashmere (__props__) and reagent (:argv) nonsense
                    argv-props (get-in props [:argv 1])
                    props (merge argv-props
                                (dissoc props :argv))]
                (log/trace "Props" props)
                (component props context))
              (catch PolyglotException pe
                (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
                  (log/error pe)))
              (catch Throwable t
                (log/error t))))
          ProxyObject
          ; Object  getMember(String key)
          ; Returns the value of the member.
          (getMember [_this k] (clj->js (get m k)))
          ; Object  getMemberKeys()
          ; Returns array of member keys.
          (getMemberKeys [_this] (into-array String (map str (keys m))))
          ; boolean   hasMember(String key)
          ; Returns true if the proxy object contains a member with the given key, or else false.
          (hasMember [_this k] (contains? m k))
          ; void  putMember(String key, Value value)
          ; Sets the value associated with a member.
          (putMember [_this k v])
          ; default boolean   removeMember(String key)
          ; Removes a member key and its value.
          (removeMember [_this k]))
        (str component)))))

(def ^Context$Builder context-builder
  (let [cwd (-> (java.io.File. ".") .getAbsolutePath)
        require-cwd (str cwd "/src")
        _ (log/info "require-cwd" require-cwd)
        builder (doto (Context/newBuilder (into-array String [language-id]))
                  (.allowExperimentalOptions true)
                  (.option "js.timer-resolution" "1")
                  (.option "js.java-package-globals" "true")
                  (.option "js.commonjs-require" "true")
                  (.option "js.commonjs-require-cwd" require-cwd)
                  (.option "js.experimental-foreign-object-prototype" "true")
                  (.out System/out)
                  (.err System/err)
                  (.allowIO true)
                  (.allowAllAccess true)
                  (.allowNativeAccess true))]
    (log/info (-> (java.io.File. ".") .getAbsolutePath))
    (if false
      (let [port "4242"
            path (str (java.util.UUID/randomUUID))
            remoteConnect "true"
            hostAdress  "localhost"
            url (format "chrome-devtools://devtools/bundled/js_app.html?ws=%s:%s/%s"
                        hostAdress port path)]
        (log/info "Debug @ " url)
        (doto builder
          (.option "inspect" port)
          #_(.option "inspect.Path" path)
          #_(.option "inspect.Remote" remoteConnect)))
      builder)))

(defn simple-clj->js
  [x]
  (if (fn? x)
    (clj->js x)
    (Value/asValue x)))

(def ^:private exec-cache (atom {}))
(defn js-fn [^Context context fn-name]
  (if-let [^Value fn-ref (-> exec-cache deref (get fn-name))]
    fn-ref
    (let [fn-ref (.eval context language-id fn-name)]
      (swap! exec-cache assoc fn-name fn-ref)
      fn-ref)))

(defn execute-fn [^Context context fn-name & args]
  "Execute Javascript function by name. Args are not marshalled. Result not marshalled.
   Javascript execution is single-threaded. Restrict concurrent access to evaluation"
  (locking ctx-lock
    (try
      (let [^Value fn-ref (js-fn context fn-name)
            arg-vals (into-array Object args)]
        (assert (.canExecute fn-ref) (str "cannot execute " fn-name))
        (.execute fn-ref arg-vals))
    (catch PolyglotException pe
      (log/error "error executing" fn-name)
      (when-let [message (.getMessage pe)]
        (log/error message))
      (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
        (log/error pe)))
    (catch Throwable t
      (log/error t)))))

(defn call-fn [^Context context fn-name & args]
  "Execute Javascript function by name. Args are marshalled. Result is marshalled"
  (js->clj (execute-fn context fn-name (map clj->js args))))


(def reflow-css-keys
  ; From https://csstriggers.com/
  #{:top
    :bottom
    :left
    :right
    :align-content
    :align-items
    :align-self
    :display
    :position
    :width
    :height
    :min-width
    :min-height
    :max-width
    :max-height
    :margin
    :margin-left
    :margin-right
    :margin-top
    :margin-bottom
    :padding
    :padding-left
    :padding-right
    :padding-top
    :padding-bottom
    ; Custom
    ; TODO: Find some documentation describing if css content changes do or do not trigger reflow
    :content
    })

(defn pre-walk-instances
  [f e]
  (if (ci/cashmere-instance? e)
    (let [new-e (f e)]
      (update new-e :children
        (fn [children-atom]
          (let [children (vec @children-atom)
                new-children (mapv (partial pre-walk-instances f)
                               children)]
            (atom
              new-children)))))
    e))

(defn pre-walk-instances-with-parent
  ([f e]
    (pre-walk-instances-with-parent f e nil))
  ([f e parent]
    (if (ci/cashmere-instance? e)
      (let [new-e (f e parent)]
        (update new-e :children
          (fn [children-atom]
            (let [children (vec @children-atom)
                  new-children (mapv (fn [child] (pre-walk-instances-with-parent f child new-e))
                                 children)]
              (atom
                new-children)))))
      e)))

(defn post-walk-instances
  [f e]
  (if (ci/cashmere-instance? e)
    (f
      (update e :children
        (fn [children-atom]
          (atom
            (mapv (partial post-walk-instances f)
               @children-atom)))))
    e))

(defn clone-instance
  [instance new-props new-children layout-required]
  (assoc instance
    :props new-props
    :children new-children
    :layout-required layout-required
    :last-children (when (= layout-required :children-changed)
                     (let [last-children (-> instance :children deref)]
                       (assert (vector? last-children))
                       last-children))))

(defn conj-child!
  [instance child]
  #_(log/debug "conj-child! parent"
    (:element-type instance)
    "key:" (get-in instance [:props :key])
    "last-children" (some-> instance :last-children count)
    "layout-required:" (get instance :layout-required)
    "child:"
    (:element-type child)
    "key=" (get-in child [:props :key])
    "layout-required:" (get child :layout-required)
    ;"last-children" (some-> child :last-children)
    "host-dom" (some-> child :host-dom deref))
  (let [child-instance
         (if-let [last-children (:last-children instance)]
           (let [last-index (-> instance :children deref count)]
             #_(log/info "last-index" last-index "last-children count" (count last-children))
             (if (< last-index (count last-children))
               (let [last-child (nth last-children last-index)]
                 #_(log/info "conj-child checking last-child" (:element-type last-child) (get-in last-child [:props :key]))
                 #_(log/info "conj-child checking new-child" (:element-type child) (get-in child [:props :key]))
                 (cond
                   (= :raw-text
                      (:element-type child)
                      (:element-type last-child))
                     (let [last-text (-> last-child :children deref first)
                           new-text (-> child :children deref first)
                           layout-required (ci/text-instance-layout-required last-text new-text)]
                       #_(log/info "last-text" last-text)
                       #_(log/info "new-text" new-text)
                       #_(log/info "layout-required" layout-required)
                       (assoc child :layout-required layout-required))
                   (not= child last-child)
                     (assoc child :layout-required :new-child)
                   :else
                     child))
               child))
           child)
           child-instance (cond->> child-instance
                                   ; Set :cloned to all children recursively if child has last-children
                                   ; ie been cloned
                                   ; FIXME: is this the right approach? it breaks word wrapping later on in zaffre
                                   (:last-children child)
                                   (pre-walk-instances
                                     (fn [instance]
                                       (log/info "cloned parent instance. Setting child :layout-required to :cloned key=" (get-in instance [:props :key]) "host-dom" (some-> instance :host-dom deref))
                                       ; TODO: should this be :cloned or something else? copy parent layout?
                                       (cond-> instance
                                         (some-> instance :host-dom deref)
                                         (assoc :layout-required :cloned)))))]
    (swap! (:children instance) conj child-instance)))

(defn reset-children!
  [instance new-children]
  (reset! (:children instance) new-children)
  instance)

(defn swap-children!
  [instance f & more]
  (apply swap! (:children instance) f more)
  instance)

(defn style-change-requires-layout?
  [old-props new-props]
  (let [old-style-reflow (-> old-props (get :style) (select-keys reflow-css-keys))
        new-style-reflow (-> new-props (get :style) (select-keys reflow-css-keys))]
    (when (not= old-style-reflow
                new-style-reflow)
      (log/info "style-change requires layout" (vec (take 2 (clojure.data/diff old-style-reflow new-style-reflow))))
      (vec (take 2 (clojure.data/diff old-style-reflow new-style-reflow)))
      #_true)))

;; Roughly taken from https://github.com/facebook/react/blob/235a6c4af67e3e1fbfab7088c857265e0c95b81f/packages/react-noop-renderer/src/createReactNoop.js
(def default-host-config
  {
    "getRootHostContext" (fn getRootHostContext [next-root-instance] {})
    "prepareForCommit" (fn prepareForCommit [& args] #_(log/trace "prepareForCommit" args))
    "resetAfterCommit" (fn resetAfterCommit [& args] (log/info "resetAfterCommit" args))
    "getChildHostContext" (fn getChildHostContext [& args] #_(log/trace "getChildHostContext" args) {})
    "shouldSetTextContent" (fn shouldSetTextContent [element-type next-props]
      #_(log/trace "shouldSetTextContent" element-type (type element-type) props)
      (or (string? element-type)
          (number? element-type)))
    "createInstance" (fn createInstance [type props root-container-instance host-context internal-instance-handle]
      ; type, props, children, host-dom, rquires-layout
      (ci/new-instance (keyword type) props))
    "createTextInstance" (fn createTextInstance [new-text root-container-instance host-context work-in-progress]
      ; TODO: cleanup
      (ci/new-text-instance {} new-text))
    "supportsPersistence" true
    ;; Persistence API
    "appendInitialChild" (fn appendInitialChild [parent child]
      #_(log/trace "appendInitialChild" parent child)
      (let [children-atom  (:children parent)]
        #_(log/info "appendInitialChild parent-layout-required:"
          (:layout-required parent)
          (type children-atom)
          (-> children-atom deref count))
        (when-not children-atom
          (log/info "children atom nil")
          (log/info "parent" parent)
          (log/info "child" child))
        ; TODO: why would parent or child be nil?
        (conj-child! parent child)))
    "finalizeInitialChildren" (fn finalizeInitialChildren [instance type new-props root-container-instance current-host-context]
      
      false)
    "createContainerChildSet" (fn createContainerChildSet [container]
      #_(log/trace "createContainerChildSet" container)
      (atom []))
    "appendChildToContainerChildSet" (fn appendChildToContainerChildSet [parent child]
      #_(log/trace "appendChildToContainerChildSet" parent child)
      (swap! parent conj child))
    "finalizeContainerChildren" (fn finalizeContainerChildren [container new-child-set]
                                  #_(log/trace "finalizeContainerChildren" @container @new-child-set)
                                  (reset! container @new-child-set))
    "replaceContainerChildren" (fn replaceContainerChildren [container new-children]
       #_(log/trace "replaceContainerChildren" (vec @container) @new-children)
       (reset! container @new-children))
    "getPublicInstance" (fn getPublicInstance [instance]
      #_(log/trace "getPublicInstance" instance)
      instance)
    "commitMount" (fn commitMount [dom-element type new-props fiber-node]
      #_(log/trace "commitMount" dom-element (keys fiber-node) (get fiber-node "stateNode")))
    "prepareUpdate" (fn prepareUpdate [instance type oldProps newProps rootContainerInstance currentHostContext]
      #_(log/trace "prepareUpdate" args)
      {})
    "cloneInstance" (fn cloneInstance [current-instance updatePayload type oldProps newProps workInProgress childrenUnchanged recyclableInstance]
      ; From https://codeclimate.com/github/facebook/react/packages/react-noop-renderer/src/createReactNoop.js/source
      (log/info "cloneInstance"  type (get newProps :key) "children unchanged" childrenUnchanged)
      (clone-instance
        current-instance
        newProps
        (if childrenUnchanged
          (:children current-instance)
          (atom []))
        (let [style-changed (style-change-requires-layout? oldProps newProps)
              children-changed (not childrenUnchanged)]
            #_(when (or style-changed children-changed)
              (log/info "checking layout required for" (keyword type) "oldkey" (get oldProps :key) "newkey" (get newProps :key) "style changed" style-changed "children changed" children-changed))
            (cond
              style-changed
                :style-changed
              children-changed
                :children-changed
              :else
                :cloned))))})

(defn argv->children
  [argv]
  #_(log/info "argv" argv (type argv))
  (if-let [children (drop 2 argv)]
    children
    []))

(defn create-element
  [component props & children]
  #_(log/info "====== React.createElement ======")
  #_(log/info "Type" component)
  #_(log/info "Props" props)
  #_(log/info "Type props" (type props))
  #_(log/info "Children" children)
  #_(log/info "Type children" (type children))
  (let [children (if children
                   children
                   (let [argv (get props :argv)]
                     (argv->children argv)))
        ^Value element (execute-fn
                   *context*
                   "React.createElement"
                   (component->js component)
                   (props->js props)
                   children)]
    #_(log/trace "element" element)
    #_(log/trace "element children" (-> element
                                   (.getMember "props")
                                   (.getMember "children")))
    element))

(defmacro with-context
  [context & body]
  `(let [^Context context# ~context]
    (binding [*context* context#
               gt/*create-element* create-element
               gt/*convert-props* (fn [props# id-class#]
                                    (props->js props#))]
       ; Nested binding form because bindings occur in parallel unlike let
       #_(log/info "context" *context*)
       ; TODO: how to typehint *context* 
       (binding [*create-symbol* (let [symbol-fn# (.eval context# language-id "Symbol")]
                                   (fn [s#]
                                     (log/info "creating symbol" s#)
                                     (execute symbol-fn# s#)))]
         (assert *context*)
         (locking context
           ~@body)))
    context#))

(defn context
  ([]
    (context default-host-config))
  ([host-config set-timeout!]
   {:post [(some? %)]}
  (try
    (let [^Context context (.build context-builder)
          source (slurp (clojure.java.io/resource 
                    "node_modules/jvm-npm/src/main/javascript/jvm-npm.js" ))
          oa-source (slurp (clojure.java.io/resource 
                      "node_modules/object-assign/index.js" ))
          cwd (-> (java.io.File. ".") .getAbsolutePath)
          require-cwd (str cwd "/src/node_modules")
          ; for event loop
          events (chan (sliding-buffer 100))]
      (assert (some? source))
      (assert (some? oa-source))
      (assert (some? context))
      ; Fake node.env.NODE_ENV
      (.eval context language-id "process = {env: {NODE_ENV:'development'}};")
      (-> context
        (.getBindings language-id)
        (.putMember "jvm_npm_source" (Value/asValue source)))
      (-> context
        (.getBindings language-id)
        (.putMember "oa_source" (Value/asValue oa-source)))
      (-> context
        (.getBindings language-id)
        (.putMember "hostConfig" (clj-map->js (merge default-host-config host-config))))
      (-> context
        (.getBindings language-id)
        (.putMember "setTimeout" (clj->js set-timeout!)))
      (assert context)
        
      ;; Adapted from https://blog.atulr.com/react-custom-renderer-1/
      (.eval context language-id
        "
        //ObjectAssign = require('object-assign');
        //load({'name': 'object-assign', 'script': oa_source});
        React = require('react');
        Reconciler = require('react-reconciler')
        reconcilerInstance = Reconciler(hostConfig);
        // Graaljs doesn't come with setTimeout by default :shrug:
        //function setTimeout(fn, timeout) {
        //  fn();
        //}
        const Renderer = {
          render(element, renderDom, callback) {
            print('=== REACT ===');
            print(element);
            print(JSON.stringify(element));
            print(setTimeout);
            print(typeof setTimeout);
            print(setTimeout instanceof Object);
            print(print instanceof Object);
            const isAsync = false;
            const container = reconcilerInstance.createContainer(renderDom, isAsync);
            const parentComponent = null;
            reconcilerInstance.updateContainer(
              element,
              container,
              parentComponent,
              function (context) { callback(context, container);});
          },
        }
        module.exports = Renderer")
      (assert context)
      context)
    (catch PolyglotException pe
      (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
        (log/error pe)))
    (catch Exception e
      (log/error e)))))

(defmacro with-context-from-host-config
  [host-config set-timeout! & body]
  `(with-context (context ~host-config ~set-timeout!) ~@body))


(defn render
  ([component props]
    (render component props (atom [])))
  ([component props container]
    (render component props container nil))
  ([component props container callback]
    #_(log/trace "====== React.render ======")
    #_(log/trace "component" component)
    #_(log/trace "props" props)
    (let [element (gt/as-element [component props])]
      #_(log/trace "render element" element)
      (execute-fn *context* "Renderer.render"
        element
        container
        (clj->js (or callback (fn default-callback [& more] (log/trace "default callback" more))))))))

(defn clj-element
  [element]
  (if (map? element)
    (let [typeof (get element "$$typeof")
          t (case (get typeof "type")
              "symbol" (keyword (get element "type")))
          props (get element "props")]
      [t (dissoc props :children) (get props :children)])
    element))

(defn clj-elements
  [element]
  #_(log/trace "element" element)
  (if (vector? element)
    (let [[t props children host-dom layout-required] element]
      [t
       (dissoc props :children)
       (mapv clj-elements (if children @children []))
       host-dom
       layout-required])
    element))


(defn render-with-context
  "Returns a function f which when invoked with f & args invokes (apply f args)
  with the bindings present."
  [component props on-render set-timeout!]
  (with-context-from-host-config
    {"resetAfterCommit" (fn [container]
      (if-let [root-element (-> container deref first)]
        (do
          #_(log/info "Rendered" (clj-elements root-element))
          (on-render root-element))
        (log/warn "root element nil")))}
    set-timeout!
    (log/info "=== Rendering ===")
    (render component props)))

(defmacro defcomponent
  [compname args & body]
  `(def ~compname (with-meta
     (fn ~compname  ~args
       (try
         (let [v# (do ~@body)]
           (gt/as-element v#))
         (catch Throwable t#
           (log/error t#))))
        {:display-name (str ~compname)})))

;; Hooks API
(defn use-state
  ([initial-state]
    (use-state initial-state []))
  ([initial-state deps]
    ; update-fn is a wrapped js function so args will need to be manually converted
    #_(log/trace "Invoked use-state with" initial-state deps)
    (let [[state update-fn!] (js->clj (execute-fn *context* "React.useState"
                                          (simple-clj->js initial-state)
                                          (clj->js deps)))
           set-state! (fn [x]
                        (if (fn? x)
                          ; x is an fn
                          ; Schedule to run on main thread
                          (go
                            #_(log/trace "In use-state set-state! go block")
                            (>! *callback-chan* (fn []
                              (update-fn! (clj->js (fn [v]
                                                     
                                                     (try
                                                       #_(log/debug "Updating using fn" v x)
                                                       (x v)
                                                       (catch Throwable t
                                                         (log/error t)))))))))
                          ; x is value
                          (update-fn! x)))]
      [state set-state!])))
  
(defn use-effect
  ([f]
    (use-effect f nil))
  ([f deps]
    ; wrap f so that it already returns a cleaup function
    ; either one supplied by f for a no-op fn
    #_(log/info "use-effect deps" (type deps) deps (type (clj->js deps)) (clj->js deps))
    ; Deps must be convertible to an array
    (assert (or (nil? deps) (sequential? deps)))
    (letfn [(effect-fn []
              (let [result (f)]
                (clj->js
                  (if (fn? result)
                    result
                    ; Rreturn an empty destroy function if response is not a function
                    (fn use-effect-default-destroy [] nil)))))]
        (log/trace "Calling React.useEffect in context" *context*)
        (js->clj (execute-fn *context* "React.useEffect" (clj->js effect-fn) (clj->js deps))))))

(defn use-context
  [c]
  (js->clj (execute-fn *context* "React.useContext" c)))

(defn use-reducer
  ([reducer initial-arg]
    (use-reducer reducer initial-arg identity))
  ([reducer initial-arg init]
   {:post [(some? %)
           (vector? %)
           (= (count %) 2)]}
    (letfn [(reducer-fn [state action]
              (simple-clj->js (reducer state action)))
            (init-fn [s]
              (Value/asValue (init s)))]
      (js->clj (execute-fn *context* "React.useReducer"
        (clj->js reducer-fn)
        (simple-clj->js initial-arg)
        (clj->js init-fn))))))

(defn use-callback
  [f deps]
  (js->clj (execute-fn *context* "React.useCallback" f deps)))

(defn use-memo
  [f deps]
  (js->clj (execute-fn *context* "React.useMemo" (clj->js f) (clj->js deps))))

(defn use-ref
  [initial-value]
  (let [r (execute-fn *context* "React.useRef" (simple-clj->js initial-value))]
    (with-meta (js->clj r) {:original r})))

(defn use-imperative-handle
  [ref createHandle deps]
  (js->clj (execute-fn *context* "React.useImperativeHandle" ref createHandle deps)))

(defn use-layout-effect
  ([f]
    (use-layout-effect f nil))
  ([f deps]
    ; wrap f so that it already returns a cleaup function
    ; either one supplied by f for a no-op fn
    #_(log/info "use-effect deps" (type deps) deps (type (clj->js deps)) (clj->js deps))
    (letfn [(effect-fn []
              (let [result (f)]
                (clj->js
                  (if (fn? result)
                    result
                    ; Rreturn an empty destroy function if response is not a function
                    (fn use-effect-default-destroy [] nil)))))]
      (js->clj (execute-fn *context* "React.useLayoutEffect" (clj->js effect-fn) (clj->js deps))))))

(defn use-debug-value [v]
  (js->clj (execute-fn *context* "React.useDebugValue" v)))

;; Demo Section
(defn thread-name
  []
  (.getName (Thread/currentThread)))

(defcomponent TestComponent
  [props context]
  (log/trace "TestComponent" props context (thread-name))
  [:ul {:key "ul"}
   (get props :children)])

(defcomponent RootComponent
  [props _]
  (let [[seconds set-seconds!] (use-state 0)]
    (use-effect (fn []
      (let [stop-chan (chan)]
        (go-loop []
          (alt!
            (async/timeout 1000)
              (do
                (try
                  (set-seconds! inc)
                  (catch Exception e
                    (log/error e)))
                (recur))
            stop-chan nil))
        (fn [] (put! stop-chan true))))
      [seconds])
    [TestComponent {}
      [:li {:key "1"} (str seconds)]
      [:li {:key "2"} 2]]))

(defn -main [& args]
  (try
    
    #_(with-context-from-host-config {}
      (js->clj (execute-fn *context*
                    "setTimeout"
                    (clj->js (fn [] (log/info "setTimeout callback")))
                    (clj->js 0))))

    (let [render-chan (chan (sliding-buffer 1))
          context (render-with-context
                    RootComponent {}
                    (partial >!! render-chan)
                    ; setTimeout
                    (fn [f t]
                      (go
                        (<! (async/timeout t))
                        (>! *callback-chan* f))))]
      ; Listen for renders and print them out
      (go-loop []
        (try
          (log/info "=== Start of render go-loop ===")
          (let [container (<! render-chan)]
            (log/info "== Render Item ==")
            (log/info "Container\n" (cu/tree->str container))
            #_(log/debug "container keys" (keys container))
            #_(log/debug "container" (-> container clj-elements)))
          (log/info "=== End of render go-loop ===")
          (catch Exception e
            (log/error e)))
        (recur))
      ; Event loop for setTimeout and setInterval
    (loop []
      (log/trace "=== Start of event loop ===")
      (let [f (<!! *callback-chan*)]
        (with-context context
          (try
            (f)
            (catch Throwable t
              (log/error t)))))
      (log/trace "=== End of event loop ===")
      (recur)))
    (catch PolyglotException pe
      (with-redefs [io.aviso.exception/expand-stack-trace expand-polygot-stack-trace]
        (log/error pe)))
    (catch Exception e
      (log/error e))))
