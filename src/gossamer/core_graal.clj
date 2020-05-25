(ns gossamer.core-graal
  (:require [gossamer.template :as gt]
            [criterium.core :as cc]
            [taoensso.timbre :as log]
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
    #_(log/trace "mo" mo)
    (when (.hasMembers mo)
      #_(log/trace "mo has members" (.getMemberKeys mo))
      (when (.hasMember mo "type")
        (when-let [t (.getMember mo "type")]
          (let [type-string (value->clj t)]
            #_(log/trace "meta-type" type-string "for" v (type t))
            #_(log/trace (contains? #{"symbol"} type-string))
            #_(log/trace (= "symbol" type-string))
            (contains? #{"symbol"} type-string)))))))

;  Returns a Clojure (or Java) value for given polyglot Value if possible,
;     otherwise throws.
(defmulti value->clj (fn [^Value v]
  #_(log/trace "v" v (type v)
    "\n.isHostObject" (.isHostObject v)
    "\n.canExecute" (.canExecute v)
    "\n.canInstantiate" (.canInstantiate v)
    ;"\nmeta-required?" (meta-required? v)
    "\n.hasArrayElement" (.hasArrayElements v)
    "\n.hasMembers" (.hasMembers v)
    "\n.isProxyObject" (.isProxyObject v))
  (let [dispatch (cond
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
                   (meta-required? v) :meta
                   (.hasArrayElements v) :array
                   (.hasMembers v) (if (.hasMember v "__props__") :props :object)
                   (.isProxyObject v) :proxy
                   :else (do
                           (print-table (:members (r/reflect v)))
                           (throw (Exception. (str "Unsupported value " v " " (type v) " " (.getMetaObject v) " " (type (.getMetaObject v)))))))]
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

(defmethod value->clj :meta
  [^Value v]
  (let [mo (.getMetaObject v)]
    #_(log/trace "mo" mo)
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
  #_(log/trace "Got props" v (type v))
  (merge (value->clj (.getMember v "__props__"))
         (into {}
           (for [k (.getMemberKeys v)
                 :when (not (= k "__props__"))
                 :let [v (.getMember v k)]]
             (let [k (get {"children" :children} k k)]
               #_(log/trace "map value" k v (type v))  
               [k (value->clj v)])))))

(defn fiber?
  [ks]
  (= #{"tag" "key" "elementType" "type" "stateNode" "return" "child" "sibling" "index" "ref" "pendingProps" "memoizedProps" "updateQueue" "memoizedState" "dependencies" "mode" "effectTag" "nextEffect" "firstEffect" "lastEffect" "expirationTime" "childExpirationTime" "alternate" "actualDuration" "actualStartTime" "selfBaseDuration" "treeBaseDuration" "_debugID" "_debugIsCurrentlyTiming" "_debugSource" "_debugOwner" "_debugNeedsRemount" "_debugHookTypes"} (set ks)))

(defmethod value->clj :object
  [^Value v]
  #_(log/trace "Got object" v (type v))
  #_(log/info "Got object ks" (.getMemberKeys v))
  (let [ks (.getMemberKeys v)
        ks (if (and true (fiber? ks))
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
        #_(log/trace "===== Executing Clojure Function ====")
        #_(log/trace "fn" x)
        (let [clj-args (mapv value->clj args)]
          #_(log/trace "clj arg " clj-args)
          (try
            (clj->value (apply x clj-args))
            (catch PolyglotException pe
              (log/error (.getPolyglotStackTrace pe)))
            (catch Throwable t
              (log/error t))))))))

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

(defmethod clj->value :keyword [x]
  (Value/asValue (str ":" (name x))))

(defmethod clj->value :default [x]
  (Value/asValue x))

(defn props->value
  [props]
  #_(log/trace "props->value" props (type props))
  (if (instance? Value props)
    props
    (Value/asValue
      (ProxyObject/fromMap
        (hash-map "__props__" (Value/asValue props)
                  "key" (get props :key))))))

(defn component->value
  [component]
  #_(log/trace "component->value" component)
  (Value/asValue 
    (if (fn? component)
      (reify ProxyExecutable
        (execute [_this args]
          #_(log/trace "===== Executing Clojure Component Function ====")
          #_(log/trace "fn" component)
          (try
            (let [[props context] (mapv value->clj args)
                  ; unwrap gossamer (__props__) and reagent (:argv) nonsense
                  props (get-in props [:argv 1])
                  props (merge props
                              (dissoc props :argv))]
              #_(log/trace "props" props)
              (component props context))
            (catch PolyglotException pe
              (log/error (.getPolyglotStackTrace pe)))
            (catch Throwable t
              (log/error t)))))
      (str component))))

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

(defn execute-fn [^Context context fn-name & args]
  ;; Javascript execution is single-threaded. Restrict concurrent access to evaluation
  (locking ctx-lock
    (try
    (let [fn-ref (.eval context language-id fn-name)
          arg-vals (into-array Object args)]
      (assert (.canExecute fn-ref) (str "cannot execute " fn-name))
      (.execute fn-ref arg-vals))
    (catch PolyglotException pe
      (log/error (.getPolyglotStackTrace pe)))
    (catch Throwable t
      (log/error t)))))

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
      #_(log/trace "createInstance" type props)
      [(keyword type) props (atom [])])
    "createTextInstance" (fn createTextInstance [new-text root-container-instance host-context work-in-progress] new-text)
    "supportsPersistence" true
    ;; Persistence API
    "appendInitialChild" (fn appendInitialChild [parent child]
      #_(log/trace "appendInitialChild" parent child)
      (-> parent (nth 2) (swap! conj child)))
    "finalizeInitialChildren" (fn finalizeInitialChildren [instance type new-props root-container-instance current-host-context] false)
    "createContainerChildSet" (fn createContainerChildSet [container]
      #_(log/trace "createContainerChildSet" container)
      (atom []))
    "appendChildToContainerChildSet" (fn appendChildToContainerChildSet [parent child]
      #_(log/trace "appendChildToContainerChildSet" parent child)
      (swap! parent conj child))
    "finalizeContainerChildren" (fn finalizeContainerChildren [container new-child-set]
                                  #_(log/trace "finalizeContainerChildren" @container @new-child-set)
                                  (swap! container concat @new-child-set))
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
    "cloneInstance" (fn cloneInstance [currentInstance updatePayload type oldProps newProps workInProgress childrenUnchanged recyclableInstance]
      ; From https://codeclimate.com/github/facebook/react/packages/react-noop-renderer/src/createReactNoop.js/source
      #_(log/info "cloneInstance"  type @(nth currentInstance 2) oldProps newProps childrenUnchanged)
      [(keyword type)
       newProps
       (if childrenUnchanged
         (nth currentInstance 2)
         (atom []))])})

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

(defn create-element
  [component props & children]
  #_(log/trace "====== React.createElement ======")
  #_(log/trace "Type" component)
  #_(log/trace "Props" props)
  #_(log/trace "Type props" (type props))
  #_(log/trace "Children" children)
  #_(log/trace "Type children" (type children))
  (let [children (if children
                   children
                   (if-let [children (-> props :argv (get 2))]
                     [children]
                     []))
        #_ (log/trace "children" children)
        ^Value element (execute-fn
                   *context*
                   "React.createElement"
                   (component->value  component)
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
    (assert *context*)
    ~@body))

(defn render-with-context
  [component props]
  (let [render-chan (chan (sliding-buffer 2))]
    (with-context
      {"resetAfterCommit" (fn [container] (>!! render-chan (-> container deref first)))}
      (log/info "=== Rendering ===")
      (render component props))
    render-chan))

(defmacro defcomponent
  [compname args & body]
  `(defn ~compname ~args
     (let [v# (do ~@body)]
       (gt/as-element v#))))

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
    (let [[t props children] element]
      [t (dissoc props :children) (mapv clj-elements (if children @children []))])
    element))


;; Hooks API
(defn use-state
  ([initial-state]
    (use-state initial-state []))
  ([initial-state deps]
    (value->clj (execute-fn *context* "React.useState" initial-state (clj->value deps)))))
  
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
  [reducer initial-arg init]
  (value->clj (execute-fn *context* "React.useReducer" reducer initial-arg init)))

(defn use-callback
  [f deps]
  (value->clj (execute-fn *context* "React.useCallback" f deps)))

(defn use-memo
  [f deps]
  (value->clj (execute-fn *context* "React.useMemo" (clj->value f) (clj->value deps))))

(defn use-ref
  [initial-value]
  (value->clj (execute-fn *context* "React.useRef" initial-value)))

(defn use-imperative-handle
  [ref createHandle deps]
  (value->clj (execute-fn *context* "React.useImperativeHandle" ref createHandle deps)))

(defn use-layout-effect [f]
  (value->clj (execute-fn *context* "React.useLayoutEffect" f)))

(defn use-debug-value [v]
  (value->clj (execute-fn *context* "React.useDebugValue" v)))

;; Demo Section
(def once (atom false))

(defn thread-name
  []
  (.getName (Thread/currentThread)))

(defcomponent TestComponent
  [props context]
  (log/trace "TestComponent" props context (thread-name))
  (let [[s set-s!] (use-state 0)]
    (use-effect
      (fn []
        (log/trace "in effect s:" s "once:" @once (thread-name))
        (future
          (log/trace "in future" (thread-name))
          (if-not @once
            (do
              (log/trace "setting s")
              (set-s! (inc s))
              (reset! once true))
            (do
              ; should never get here
              (log/error "ERROR: this should never be called")
              (assert false)))))
      
      [])
  (log/info "TestComponent end" props context s (thread-name))
    [:ul {:key "ul"}
     [:li {:key "li"} (str s)]]))

(defn -main [& args]
  (try
    (let [render-chan (render-with-context TestComponent {})]
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