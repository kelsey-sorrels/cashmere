(ns gossamer.core-test
  (:require [clojure.test :refer :all]
            clojure.pprint
            [clojure.core.async :as async :refer [<!!]]
            [taoensso.timbre :as log]
            [gossamer.core :refer :all]))

(deftest create-element-test
  (testing "create-element with no children"
    (is (= (create-element :elem {})
           (create-element :elem {}))))
  (testing "create-element with 1 child"
    (is (= (create-element :elem {} :a)
           (create-element :elem {} :a))))
  (testing "create-element with text child"
    (is (= (create-element :elem {} "hello")
           (create-element :elem {} "hello"))))
  (testing "create-element with 2 children"
    (is (= (create-element :elem {} :a "hello")
           (create-element :elem {} :a "hello")))))

(defrecord NodeLogger [log]
  HostConfig
  (create-instance [this type props root-container-instance host-context internal-instance-handle]    {:type type :props props})
  (create-node [this node parent]
    (dosync (alter log conj [:create-node node parent])))
  (update-node [this node prev-props next-props]
    (dosync (alter log conj [:update-node node prev-props next-props])))
  (delete-node [this node parent]
    (dosync (alter log conj [:delete-node node parent]))))

(defn node-logger
  []
  (NodeLogger. (ref []
    #_#_:validator (fn [log] (seq log)))))

(def counter-atom (atom false))

(defn counter
  [props]
  (let [[state set-state!] (use-state 1)]
    (future
      (Thread/sleep 1)
      (set-state! 2))
    (create-element :counter {}
      (str "count: " state))))
  

(deftest render-element-test
  (testing "render-single-element"
    (let [logger (node-logger)
          context-ref (new-context-ref)
          render-chan (work-loop
                        context-ref
                        logger)]
      (render-sync
        render-chan
        context-ref
        (create-element :parent-elem {}
          (create-element :child-elem-1 {})
          (create-element :child-elem-2 {}))
        ; container
        [])
      (log/info "====== Second Render ======")
      (render-sync
        render-chan
        context-ref
        (create-element :parent-elem {}
          (create-element :child-elem-1 {:prop 1})
          (create-element counter {}))
        [])
      (log/info "====== Third Render ======")
      (render-sync
        render-chan
        context-ref
        (create-element :parent-elem {}
          (create-element :child-elem-1 {:prop 1})
          (create-element counter {}))
        [])
      (log/info "log:\n" (with-out-str (clojure.pprint/pprint (-> logger :log deref))))
      (Thread/sleep 20)
      (is (= (-> logger :log deref) [])))))

(deftest render-function-element-test
  (testing "render function element"
    (let [logger (node-logger)
          context-ref (new-context-ref)
          render-chan (work-loop
                        context-ref
                        logger)]
      (render-sync
        render-chan
        context-ref
        (create-element counter {})
        :container)
      #_(log/info "====== Second Render ======")
      #_(render-sync
        render-chan
        context-ref
        (create-element :parent-elem {}
          (create-element :child-elem-1 {:prop 1})
          (create-element counter {}))
        [])
      (log/info "log:\n" (with-out-str (clojure.pprint/pprint (-> logger :log deref))))
      (Thread/sleep 20)
      (is (= (-> logger :log deref) [])))))
  
