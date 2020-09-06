(ns cashmere.core-graal
  (:require [cashmere.template :as gt]
            [criterium.core :as cc]
            [taoensso.timbre :as log]
            [clojure.data]
            [clojure.inspector]
            [clojure.java.io]
            [clojure.core.async :refer [chan go go-loop sliding-buffer <!! <! >! >!!]])
  (:import (org.graalvm.polyglot Context Context$Builder PolyglotException Source Value)
           (org.graalvm.polyglot.proxy ProxyArray ProxyExecutable ProxyObject)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def language-id "js")
(def ^:dynamic *context* nil)

(declare clj->value)
(declare value->clj)

;; Empty object for context locking
;; Locking on the Graal context itself did not work
;; TODO findout why
(def ctx-lock {})

;; Execute a js function
(defn- execute
  [^Value execable & args]
  (locking ctx-lock
    (try
      (let [result (.execute execable (object-array args))]
        (value->clj result))
      (catch PolyglotException pe
        (log/error (.getPolyglotStackTrace pe)))
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
          (let [type-string (value->clj t)]
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
(defmulti value->clj (fn [^Value v]
  #_(log/info "value->clj" v (type v)
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

(defmethod value->clj :nil
  [^Value v]
  nil)

(defmethod value->clj :host-object
  [^Value v]
  ;(log/trace "host-object" (.asHostObject v) (type (.asHostObject v)))
  (.asHostObject v))

(defmethod value->clj :boolean
  [^Value v]
  (.asBoolean v))

(defmethod value->clj :string
  [^Value v]
  (let [s (.asString v)]
    (if (clojure.string/starts-with? s ":")
      (keyword (subs s 1))
      s)))

(defmethod value->clj :number
  [^Value v]
  (.as v Number))

(defmethod value->clj :fn
  [^Value v]
  (reify-ifn v))

(defmethod value->clj :symbol
  [^Value v]
  (keyword (str v)))

(defmethod value->clj :meta
  [^Value v]
  (let [mo (.getMetaObject v)]
    (log/info "value->clj mo" mo)
    #_(when (.hasMembers v)
      (doseq [k (.getMemberKeys v)]
        (log/trace k (.getMember v k))))
    ; Turn meta objects into clj
    (value->clj mo)))

(defmethod value->clj :array
  [^Value v]
  (into []
        (for [i (range (.getArraySize v))]
          (value->clj (.getArrayElement v i)))))

(defmethod value->clj :props
  [^Value v]
  #_(log/info "Got props" v (type v))
  (let [internal-props (value->clj (.getMember v "__props__"))
        react-props (into {}
                       (for [k (.getMemberKeys v)
                             :when (not (= k "__props__"))
                             :let [prop-v (.getMember v k)]]
                         (let [k (get {"children" :children} k k)]
                           #_(log/info "map value" k prop-v (type prop-v))  
                           [k (value->clj prop-v)])))
        props (merge internal-props
                     react-props)]
    #_(log/info "internal-props" internal-props)
    #_(log/info "react-props children " (get react-props :children))
    #_(log/info "Resulting props children " (get props :children))
    props))

(defn fiber?
  [ks]
  (= #{"tag" "key" "elementType" "type" "stateNode" "return" "child" "sibling" "index" "ref" "pendingProps" "memoizedProps" "updateQueue" "memoizedState" "dependencies" "mode" "effectTag" "nextEffect" "firstEffect" "lastEffect" "expirationTime" "childExpirationTime" "alternate" "actualDuration" "actualStartTime" "selfBaseDuration" "treeBaseDuration" "_debugID" "_debugIsCurrentlyTiming" "_debugSource" "_debugOwner" "_debugNeedsRemount" "_debugHookTypes"} (set ks)))

(defmethod value->clj :object
  [^Value v]
  #_(log/trace "Got object" v (type v))
  #_(log/info "Got object ks" (.getMemberKeys v))
  (let [ks (.getMemberKeys v)
        ks (if (fiber? ks)
             #{"elementType" "type"}
             ks)]
    (into {}
      (for [k ks]
        ; Don't recurse up to the parent
        [k
         (if (contains? #{"return"
                          "child"
                          ; one of these
                          #_"elementType"
                          #_"type"
                          #_"stateNode"
                          #_"tag"
                          #_"key"
                          #_"current"
                          "_owner"
                          "_debugOwner" "nextEffect" "next"
                          "pendingProps" "memoizedProps"
                          } k)
             v
             (value->clj (.getMember v k)))]))))

(defmethod value->clj :proxy
  [^Value v]
  (.asProxyObject v))

(defmulti clj->value (fn [x] 
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
(defmethod clj->value :nil [x] (Value/asValue x))
(defmethod clj->value :fn [x]
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
        (let [clj-args (mapv value->clj args)]
          #_(log/trace "clj arg " clj-args)
          (try
            (clj->value (apply x clj-args))
            (catch PolyglotException pe
              (log/error (.getPolyglotStackTrace pe))
              (throw pe))
            (catch Throwable t
              (log/error t)
              (throw t))))))))

(defn clj-map->value
  [m]
  (Value/asValue
    (reify ProxyObject
      ; Object  getMember(String key)
      ; Returns the value of the member.
      (getMember [_this k] (clj->value (get m k)))
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

(defmethod clj->value :map [x]
  #_(log/trace "map object" x)
  (Value/asValue
    (apply hash-map
      (mapcat (fn [[k v]]
        [k #_(clj->value k)
         (clj->value v)])
        x))))

(defn clj-seq->value
  [s]
  (let [v (vec s)]
    (Value/asValue
      (reify ProxyArray
        ;get(long index)
        ;Returns the element at the given index.
        (get [_this index] (clj->value (nth v index)))
        ;long  getSize()
        (getSize [_this] (count v))
        ;Returns the reported size of the array.
        ;default boolean   remove(long index)
        (remove [_this index] false)
        ;Removes the element at the given index.
        ;void  set(long index, Value value)
        (set [_this index value])))))

(defmethod clj->value :seq [x]
  (let [v (vec x)]
  (Value/asValue
    ;(ProxyArray/fromList
      ;(java.util.ArrayList.
        (mapv clj->value v))))

(def ^:dynamic *create-symbol* nil)
(defmethod clj->value :keyword [x]
  (log/info "create symbol" *create-symbol*)
  (if *create-symbol*
    (*create-symbol* (name x))
    (Value/asValue (str ":" (name x)))))

(defmethod clj->value :default [x]
  (Value/asValue x))

(defn props->value
  [props]
  #_(log/trace "props->value" props (type props))
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

(defn component->value
  [component]
  #_(log/trace "component->value" component)
  (let [m {"displayName" (or (some-> component meta :display-name str) "Unknown")}]
    (Value/asValue 
      (if (fn? component)
        (reify
          ProxyExecutable
          (execute [_this args]
            (log/trace "===== Executing Clojure Component Function ====")
            (log/trace "fn" component)
            (try
              (let [[props context] (mapv value->clj args)
                    ; unwrap cashmere (__props__) and reagent (:argv) nonsense
                    argv-props (get-in props [:argv 1])
                    props (merge argv-props
                                (dissoc props :argv))]
                (log/trace "Props" props)
                (component props context))
              (catch PolyglotException pe
                (log/error (.getPolyglotStackTrace pe)))
              (catch Throwable t
                (log/error t))))
          ProxyObject
          ; Object  getMember(String key)
          ; Returns the value of the member.
          (getMember [_this k] (clj->value (get m k)))
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
  (let [builder (doto (Context/newBuilder (into-array String [language-id]))
     (.option "js.timer-resolution" "1")
     (.option "js.java-package-globals" "true")
     (.out System/out)
     (.err System/err)
     (.allowAllAccess true)
     (.allowNativeAccess true))]
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

(defn simple-clj->value
  [x]
  (if (fn? x)
    (clj->value x)
    (Value/asValue x)))

(def ^:private exec-cache (atom {}))
(defn execute-fn [^Context context fn-name & args]
  ;; Javascript execution is single-threaded. Restrict concurrent access to evaluation
  (locking ctx-lock
    (try
    (let [fn-ref (if-let [fn-ref (-> exec-cache deref (get fn-name))]
                   fn-ref
                   (let [fn-ref (.eval context language-id fn-name)]
                     (swap! exec-cache assoc fn-name fn-ref)
                     fn-ref))
          arg-vals (into-array Object args)]
      (assert (.canExecute fn-ref) (str "cannot execute " fn-name))
      (.execute fn-ref arg-vals))
    (catch PolyglotException pe
      (log/error "error executing" fn-name)
      (when-let [message (.getMessage pe)]
        (log/error message))
      (log/error (.getPolyglotStackTrace pe)))
    (catch Throwable t
      (log/error t)))))


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

(defrecord Instance [element-type props children host-dom layout-required last-children style-map]
  Object
  (toString [this]
    (select-keys this [:element-type :props :children])))

(defn new-instance
  ([element-type props]
    (new-instance element-type props []))
  ([element-type props children]
    (->Instance
      element-type
      props
      (atom children)
      (atom nil)
      :new-instance
      nil
      nil)))

(defn spaces-same?
  [s1 s2]
  (= (->> s1
       (re-seq #"\\s")
       (map count))
     (->> s1
       (re-seq #"\\s")
       (map count))))

(defn text-instance-layout-required
  [last-text new-text]
  (cond
    ; no change in text
    (= last-text new-text)
      :text-same
    ; same length and spaces same?
    (and
      (= (count last-text)
         (count new-text))
      (spaces-same? last-text new-text))
      :text-updated
    ; diff length or diff spaces
    :else
      :text-changed))

(defn new-text-instance
  ([props text]
    (new-text-instance props text nil))
  ([props text old-text]
    #_(log/info "new-text-instance old-text:" old-text)
    (->Instance
      :raw-text
      props
      (atom [text])
      (atom nil)
      (if old-text
        (text-instance-layout-required old-text text)
        :new-text-instance)
      false
      nil)))

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
  #_(log/info "conj-child!" (:element-type instance) (:element-type child) "last-children" (some-> instance :last-children count))
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
                           layout-required (text-instance-layout-required last-text new-text)]
                       #_(log/info "last-text" last-text)
                       #_(log/info "new-text" new-text)
                       #_(log/info "layout-required" layout-required)
                       (assoc child :layout-required layout-required))
                   (not= child last-child)
                     (assoc child :layout-required :new-child)
                   :else
                     child))
               child))
           child)]
    (swap! (:children instance) conj child-instance)))

(defn reset-children!
  [instance new-children]
  (reset! (:children instance) new-children)
  instance)

(defn swap-children!
  [instance f & more]
  (apply swap! (:children instance) f more)
  instance)

(defn pre-walk-instances
  [f e]
  (if (instance? Instance e)
    (-> e
      f
      (update :children
        (fn [children-atom]
          (let [children (vec @children-atom)
                new-children (mapv (partial pre-walk-instances f)
                   children)]
            (atom
              new-children)))))
    e))
  
(defn post-walk-instances
  [f e]
  (if (instance? Instance e)
    (-> e
      (update :children
        (fn [children-atom]
          (atom
            (mapv (partial post-walk-instances f)
                 @children-atom))))
      f)
    e))

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
      (log/info "createInstance" type props)
      ; type, props, children, host-dom, rquires-layout
      (new-instance (keyword type) props))
    "createTextInstance" (fn createTextInstance [new-text root-container-instance host-context work-in-progress]
      ; TODO: cleanup
      (new-text-instance {} new-text))
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
      #_(log/info "cloneInstance"  type @(:children current-instance) oldProps newProps childrenUnchanged)
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

(defn context
  ([]
    (context default-host-config))
  ([host-config]
   {:post [(some? %)]}
  (try
    (let [^Context context (.build context-builder)
          source (slurp (clojure.java.io/resource 
                   "node_modules/jvm-npm/src/main/javascript/jvm-npm.js" ))
         oa-source (slurp (clojure.java.io/resource 
                   "node_modules/object-assign/index.js" ))]
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
        (.putMember "hostConfig" (clj-map->value (merge default-host-config host-config))))
      (assert context)
      ;; Adapted from https://blog.atulr.com/react-custom-renderer-1/
      (.eval context language-id
        "
        load({'name': 'jvm_npm', 'script': jvm_npm_source});
        React = require('node_modules/react/cjs/react.development.js');
        Reconciler = require('node_modules/react-reconciler/cjs/react-reconciler.development.js')
        reconcilerInstance = Reconciler(hostConfig);
        // Graaljs doesn't come with setTimeout by default :shrug:
        function setTimeout(fn, timeout) {
          fn();
        }
        const Renderer = {
          render(element, renderDom, callback) {
            //print('=== REACT ===');
            //print(element);
            //print(JSON.stringify(element));
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
      (log/error (.getMessage pe))
      (log/error (.getPolyglotStackTrace pe)))
    (catch Exception e
      (log/error e)))))

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
        (clj->value (or callback (fn default-callback [& more] (log/trace "default callback" more))))))))

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
                   (component->value component)
                   (props->value props)
                   children)]
    #_(log/trace "element" element)
    #_(log/trace "element children" (-> element
                                   (.getMember "props")
                                   (.getMember "children")))
    element))

(defmacro with-context
  [host-config & body]
  `(binding [*context* (context ~host-config)
             gt/*create-element* create-element
             gt/*convert-props* (fn [props# id-class#]
                                  (props->value props#))]
     ; Nested binding form because bindings occur in parallel unlike let
     ; TODO: how to typehint *context* 
     (binding [*create-symbol* (let [symbol-fn# (-> *context* ^Value (.eval language-id "Symbol"))]
                                 (fn [s#]
                                   (log/info "creating symbol" s#)
                                   (execute symbol-fn# s#)))]
       (assert *context*)
       ~@body)))

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
  [component props on-render]
  (with-context
    {"resetAfterCommit" (fn [container]
      (if-let [root-element (-> container deref first)]
        (do
          #_(log/info "Rendered" (clj-elements root-element))
          (on-render root-element))
        (log/warn "root element nil")))}
    (log/info "=== Rendering ===")
    (render component props)
    (bound-fn* (fn [f & args] (apply f args)))))

(defmacro defcomponent
  [compname args & body]
  `(def ~compname (with-meta (fn ~compname  ~args
     (let [v# (do ~@body)]
       (gt/as-element v#))) {:display-name (str ~compname)})))

;; Hooks API
(defn use-state
  ([initial-state]
    (use-state initial-state []))
  ([initial-state deps]
    (let [[state update-fn] (value->clj (execute-fn *context* "React.useState"
      (simple-clj->value initial-state)
      (clj->value deps)))]
      [state (fn [x]
               (if (fn? x)
                 (update-fn (clj->value (fn [v]
                                          (try
                                            (x v)
                                            (catch Throwable t
                                              (log/error t))))))
                 (update-fn x)))])))
  
(defn use-effect
  ([f]
    (use-effect f nil))
  ([f deps]
    ; wrap f so that it already returns a cleaup function
    ; either one supplied by f for a no-op fn
    #_(log/info "use-effect deps" (type deps) deps (type (clj->value deps)) (clj->value deps))
    (letfn [(effect-fn []
              (let [result (f)]
                (clj->value
                  (if (fn? result)
                    result
                    ; Rreturn an empty destroy function if response is not a function
                    (fn use-effect-default-destroy [] nil)))))]
      (value->clj (execute-fn *context* "React.useEffect" (clj->value effect-fn) (clj->value deps))))))

(defn use-context
  [c]
  (value->clj (execute-fn *context* "React.useContext" c)))

(defn use-reducer
  ([reducer initial-arg]
    (use-reducer reducer initial-arg identity))
  ([reducer initial-arg init]
   {:post [(some? %)
           (vector? %)
           (= (count %) 2)]}
    (letfn [(reducer-fn [state action]
              (simple-clj->value (reducer state action)))
            (init-fn [s]
              (Value/asValue (init s)))]
      (value->clj (execute-fn *context* "React.useReducer"
        (clj->value reducer-fn)
        (simple-clj->value initial-arg)
        (clj->value init-fn))))))

(defn use-callback
  [f deps]
  (value->clj (execute-fn *context* "React.useCallback" f deps)))

(defn use-memo
  [f deps]
  (value->clj (execute-fn *context* "React.useMemo" (clj->value f) (clj->value deps))))

(defn use-ref
  [initial-value]
  (let [r (execute-fn *context* "React.useRef" (simple-clj->value initial-value))]
    (with-meta (value->clj r) {:original r})))

(defn use-imperative-handle
  [ref createHandle deps]
  (value->clj (execute-fn *context* "React.useImperativeHandle" ref createHandle deps)))

(defn use-layout-effect
  ([f]
    (use-layout-effect f nil))
  ([f deps]
    ; wrap f so that it already returns a cleaup function
    ; either one supplied by f for a no-op fn
    #_(log/info "use-effect deps" (type deps) deps (type (clj->value deps)) (clj->value deps))
    (letfn [(effect-fn []
              (let [result (f)]
                (clj->value
                  (if (fn? result)
                    result
                    ; Rreturn an empty destroy function if response is not a function
                    (fn use-effect-default-destroy [] nil)))))]
      (value->clj (execute-fn *context* "React.useLayoutEffect" (clj->value effect-fn) (clj->value deps))))))

(defn use-debug-value [v]
  (value->clj (execute-fn *context* "React.useDebugValue" v)))

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
  [TestComponent {}
    [:li {:key "1"} 1]
    [:li {:key "2"} 2]])

(defn -main [& args]
  (try
    (let [render-chan (render-with-context RootComponent {} (chan (sliding-buffer 1)))]
      ; Listen for renders and print them out
      (go-loop []
        (let [container (<! render-chan)]
          (log/info "=== Render Callback ===")
          (log/info "container" (-> container clj-elements)))
        (recur)))
    (catch PolyglotException pe
      (log/error (.getPolyglotStackTrace pe)))
    (catch Exception e
      (log/error e))))
