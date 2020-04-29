(ns gossamer.core-graal
  (:require [gossamer.template :as gt]
            [taoensso.timbre :as log]
            [clojure.java.io])
  (:import (org.graalvm.polyglot Context PolyglotException Source Value)
           (org.graalvm.polyglot.proxy ProxyArray ProxyExecutable ProxyObject)))

(def language-id "js")
(def ^:dynamic *context* nil)

(defn- execute
  [^Value execable & args]
  (.execute execable (object-array args)))

;; From https://gist.github.com/taylorwood/bb3ebfec5d5de3cccc867a9eba216c18
(declare value->clj)
(defmacro ^:private reify-ifn
  "Convenience macro for reifying IFn for executable polyglot Values."
  [v]
  (let [invoke-arity
        (fn [n]
          (let [args (map #(symbol (str "arg" (inc %))) (range n))]
            (if (seq args)
              `(~'invoke [this# ~@args] (value->clj (execute ~v ~@args)))
              `(~'invoke [this#] (value->clj (execute ~v))))))]
    `(reify clojure.lang.IFn
       ~@(map invoke-arity (range 22))
       (~'applyTo [this# args#] (value->clj (apply execute ~v args#))))))

(require '[clojure.reflect :as r])
(use '[clojure.pprint :only [print-table]])

(defn value->clj
  "Returns a Clojure (or Java) value for given polyglot Value if possible,
     otherwise throws."
  ([^Value v]
    (value->clj v 0))
  ([^Value v indent]
  (cond
    (.isNull v) nil
    (.isHostObject v) (.asHostObject v)
    (.isBoolean v) (.asBoolean v)
    (.isString v) (.asString v)
    (.isNumber v) (.as v Number)
    (.canExecute v) (reify-ifn v)
    (.hasArrayElements v) (into []
                                (for [i (range (.getArraySize v))]
                                  (value->clj (.getArrayElement v i) (inc indent))))
    (.hasMembers v) (into {}
                          (for [k (.getMemberKeys v)]
                            ; Don't recurse up to the parent
                            (do
                            ;(log/info (apply str (repeat indent "  ")) k)
                            [k (if (contains? #{"return"
                                                 "child"
                                                 ; one of these
                                                 #_"elementType"
                                                 #_"type"
                                                 #_"stateNode"
                                                 #_"tag"
                                                 #_"key"
                                                 #_"current"
                                                 "_debugOwner" "nextEffect" "next"
                                                 } k)
                                 v
                                 (value->clj (.getMember v k) (inc indent)))])))
    (.canInstantiate v) (assert false)
    (.isException v) (assert false)
    (.isTimeZone v) (assert false)
    (.isDuration v) (assert false)
    (.isNativePointer v) (assert false)
    (.isDate v) (assert false)
    (.isTime v) (assert false)
    (.isProxyObject v) (assert false)
    (.getMetaObject v) (let [mo (.getMetaObject v)]
                         ; Turn meta objects into clj
                         (value->clj mo (inc indent)))
    :else (do
            (print-table (:members (r/reflect v)))
            (throw (Exception. (str "Unsupported value " v " " (type v) " " (.getMetaObject v) " " (type (.getMetaObject v)))))))))

(defn proxy-fn
  "Returns a ProxyExecutable instance for given function, allowing it to be
     invoked from polyglot contexts."
  [f]
  (reify ProxyExecutable
    (execute [_this args]
      (apply f (map value->clj args)))))

(defn clj->value
  [x]
  (cond
    (nil? x) (Value/asValue x)
    (fn? x) (Value/asValue (proxy-fn x))
    (map? x) (Value/asValue
               (ProxyObject/fromMap
                 (apply hash-map
                   (mapcat (fn [[k v]]
                     [k
                      (clj->value v)])
                     x))))
    (seq? x) (Value/asValue
               (ProxyArray/fromList
                 (java.util.ArrayList.
                   (map clj->value x))))
    (keyword? x) (Value/asValue (name x))
    :else (Value/asValue x)))

(def context-builder
  (doto (Context/newBuilder (into-array String [language-id]))
     (.option "js.timer-resolution" "1")
     (.option "js.java-package-globals" "true")
     (.out System/out)
     (.err System/err)
     (.allowAllAccess true)
     (.allowNativeAccess true)))

(defn execute-fn [context fn & args]
  (let [fn-ref (.eval context language-id fn)
        args (into-array Object (map clj->value args))]
    (assert (.canExecute fn-ref) (str "cannot execute " fn))
    (.execute fn-ref args)))

(def default-host-config
  {
    "getRootHostContext" (fn [next-root-instance] next-root-instance)
    "prepareForCommit" (fn [& args] (log/info "prepareForCommit" args))
    "resetAfterCommit" (fn [& args] (log/info "resetAfterCommit" args))
    "getChildHostContext" (fn [& args] (log/info "getChildHostContext" args))
    "shouldSetTextContent" (fn [type next-props] true)
    "createInstance" (fn [type props root-container-instance host-context internal-instance-handle] [(keyword type) props])
    "createTextInstance" (fn [new-text root-container-instance host-context work-in-progress] new-text)
    "supportsPersistence" true
    "appendInitialChild" (fn [& args] (log/info "appendInitialChild" args))
    "finalizeInitialChildren" (fn [instance type new-props root-container-instance current-host-context] true)
    "createContainerChildSet" (fn [& args] (atom []))
    "appendChildToContainerChildSet" (fn [parent child] (swap! parent conj child))
    "finalizeContainerChildren" (fn [container new-child-set]
                                  (log/info "finalizeContainerChildren" container new-child-set)
                                  (swap! container concat @new-child-set))
    "replaceContainerChildren" (fn [& args] (log/info "replaceContainerChildren" args))
    "getPublicInstance" (fn [& args] (log/info "getPublicInstance" args))
    "commitMount" (fn [dom-element type new-props fiber-node] (log/info "commitMount" dom-element (keys fiber-node) (get fiber-node "stateNode")))})

;(log/info "host-config" host-config)

(defn context
  [host-config]
  (try
    (let [context (.build context-builder)]
      ; Fake node.env.NODE_ENV
      (.eval context language-id "process = {env: {NODE_ENV:'development'}};")
      (-> context
        (.getBindings language-id)
        (.putMember "hostConfig" (clj->value (merge default-host-config host-config))))
      (.eval context language-id
        "
        load('node_modules/jvm-npm/src/main/javascript/jvm-npm.js');
        React = require('react');
        Reconciler = require('react-reconciler')
        reconcilerInstance = Reconciler(hostConfig);
        // Graaljs doesn't come with setTimeout by default :shrug:
        function setTimeout(fn, timeout) {
          fn();
        }
        const Renderer = {
          render(element, renderDom, callback) {
            const isAsync = false;
            const container = reconcilerInstance.createContainer(renderDom, isAsync);
            const parentComponent = null;
            print('type of callback ' + typeof callback);
            reconcilerInstance.updateContainer(
              element,
              container,
              parentComponent,
              function (context) { return callback(context);});
          },
        }
        module.exports = Renderer")
      context)
    (catch Exception e
      (if (and false (instance? PolyglotException e))
        (log/error (.getPolyglotStackTrace e))
        (log/error e)))))

(defn render
  [component props & [callback]]
  (execute-fn *context* "Renderer.render"
    (gt/as-element [component props])
    (atom [])
    (or callback (fn [& more] (log/info "default callback" more)))))

(defmacro with-context
  [& body]
  `(binding [*context* (context)
             gt/*create-element* (fn [component# props# & children#]
                                   (execute-fn *context* "React.createElement" component# props# children#))]
     ~@body))

(defmacro defcomponent
  [compname args & body]
  `(defn ~compname ~args
     (let [v# (do ~@body)]
      (gt/as-element v#))))

(defcomponent counter
  [props context]
  (log/info "Counter" props context)
  [:h1 {} "Hello React"])

(defn -main [& args]
  (try
    (let [container (atom [])]
      (with-context
        (render counter {}))
      (log/info "container after render" container @container))
    (catch Exception e
      (if (and false (instance? PolyglotException e))
        (log/error (.getPolyglotStackTrace e))
        (log/error e)))))
