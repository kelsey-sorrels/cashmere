(ns gossamer.core
  (:require [clojure.string]
            #?(:clj [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt! go go-loop]]
               :cljs [clojure.core.async :as async :refer [<! >! timeout chan alt! go go-loop]])
            [clojure.pprint]
            [taoensso.timbre :as log]))

(defn pp
  [x]
  (with-out-str (clojure.pprint/pprint x)))

(defrecord Element [type props])

(defn create-text-element
  [text]
  (Element.
    ::TEXT_ELEMENT
    {
      "nodeValue" text
      :children []
    }))

; public
(defn create-element
  [type props & children]
  {:pre [type props]}
  (Element.
    type
    (assoc
      props,
      :children
        (map (fn [child]
               (if-not (string? child)
                 child
                 (create-text-element child)))
          children))))

(defprotocol HostConfig
  (create-instance [this type props root-container-instance host-context internal-instance-handle])
  (create-node [this node parent])
  (update-node [this node prev-props next-props])
  (delete-node [this node parent]))

(defn createDom
  [host-config fiber-ref]
  (let [dom (if (= (:type @fiber-ref) ::TEXT_ELEMENT)
                {:type :text-dom
                 :props (:props @fiber-ref)}
                {:type (:type @fiber-ref)
                 :props (:props @fiber-ref)}
            )]
    (update-node host-config dom {} (:props @fiber-ref))
    dom))

(defn event?
  [key]
  (clojure.string/starts-with? (str key) "on"))

(defn property?
  [key]
  (and (not= key :children)
       (not (event? key))))

(defn new?
  [prev next]
  (fn [key]
    (not= (get prev key)
          (get next key))))

(defn gone?
  [prev next]
  (fn [key]
    (not (contains? next key))))

(defn commit-deletion
  [host-config fiber-ref domParent]
  (if-let [dom (some-> fiber-ref deref :dom)]
    ;domParent.removeChild(fiber.dom)
    (delete-node host-config dom domParent)
    ; commit deletion of :child
    (commit-deletion host-config (some-> fiber-ref deref :child-ref) domParent)))

(defn commit-work
  [host-config fiber-ref]
  ;(log/info "commit-work" (some-> fiber-ref deref pp))
  (when (some-> fiber-ref deref)
    (log/info "commit-work fiber-ref")
    (let [domParentFiberRef (loop [domParentFiberRef (:parent-ref @fiber-ref)]
                          (if-not (some-> domParentFiberRef deref :dom)
                            (recur (some-> domParentFiberRef deref :parent-ref))
                            domParentFiberRef))
          domParent (-> domParentFiberRef deref :dom)]
      (log/info "domParent" domParent)
      (log/info "effectTag" (some-> fiber-ref deref :effectTag))
      (log/info "dom?" (some-> fiber-ref deref :dom))
      ;(log/info "fiber-ref\n" (pp fiber-ref))
      (let [recurse (cond
                      (and
                        (= (some-> fiber-ref deref :effectTag) "PLACEMENT")
                        (some-> fiber-ref deref :dom))
                        ; Place dom
                        ;domParent.appendChild(fiber.dom)
                        (do
                          (create-node host-config (some-> fiber-ref deref :dom) domParent)
                          true)
                      (and
                        (= (some-> fiber-ref deref :effectTag) "UPDATE")
                        (some-> fiber-ref deref :dom))
                        ; update dom
                        (do
                          (update-node
                            host-config
                            (some-> fiber-ref deref :dom)
                            (some-> fiber-ref deref :alternate-ref :props)
                            (some-> fiber-ref deref :props))
                          true)
                      (= (some-> fiber-ref deref :effectTag) "DELETION")
                       ; delete
                       (do
                         (commit-deletion host-config fiber-ref domParent)
                         ; don't recurse after deletions
                         false)
                      :else
                        ; A functional componenent, just recurse
                        true)]
        (when recurse
          ; recurse
          ; child
          (commit-work host-config (-> fiber-ref deref :child-ref))
          ; sibling
          (commit-work host-config (-> fiber-ref deref :sibiling-ref)))))))

(defrecord Fiber [type props dom parent-ref child-ref sibling-ref alternate-ref effectTag])

#?(:clj 
  (defn to-hex
    [n]
    #_(clojure.string/format "%x" n))
  :cljs 
  (defn to-hex
    [n]
    (. n (toString 16))))

(defn ref-loc
  [r]
  (when r
    [(str "<Atom@" (to-hex #?(:clj (System/identityHashCode r)
                              :cljs 0)))
     (dissoc @r :parent-ref :child-ref :sibling-ref)]))

#?(:clj
  (. clojure.pprint/simple-dispatch addMethod Fiber
    (fn [fiber]
      (clojure.pprint/pprint  (into {} (update fiber :parent-ref ref-loc)))))
  :cljs
  (defmethod clojure.pprint/simple-dispatch Fiber
    [fiber]
    (clojure.pprint/pprint  (into {} (update fiber :parent-ref ref-loc)))))

(defn new-fiber-ref [type props dom parent-ref alternate-ref effectTag]
  {:pre [props]}
  (atom
    (Fiber. 
      type
      props
      dom
      parent-ref
      ;child-ref
      nil
      ;sibling-ref
      nil
      alternate-ref
      effectTag)
  :validator (fn [fiber]
    (:props fiber))))
   

;wip-root = Fiber 
(defrecord Context [
  next-unit-of-work-ref
  current-root-ref
  wip-root-ref
  deletions
  wip-fiber-ref
  hook-index])

(defn new-context-ref
  []
  (atom (Context. nil nil nil [] nil 0)
    #_#_:validator (fn [context]
                 (println "WIP ROOT IS " (some-> context :wip-root-ref some?))
                 true)))

;; put wip root fibers onto to initiate render
(defonce work-chan
  (chan))

; Public
(defn render
  [context-ref element container]
  ; create fiber for root
  (let [wip-root-ref (new-fiber-ref
          ; type
          nil
          ;props
          {:children [element]}
          ;dom
          container
          ; parent-ref
          nil
          ;alternate-ref
          (-> context-ref deref :current-root-ref)
          ; effectTag
          nil)]
    #?(:clj
         (go
           (>! work-chan wip-root-ref))
       :cljs
         (swap! context-ref
           assoc
           :wip-root-ref wip-root-ref
           :deletions []
           :next-unit-of-work-ref wip-root-ref))))

#?(:clj
  (defn render-sync
    [render-chan context-ref element container]
    (render context-ref element container)
    (<!! render-chan)))

(defn commitRoot
  [host-config context-ref]
  (let [context @context-ref]
    ;(log/info "commit-root deletions" (-> context :deletions pp))
    (doseq [deletion (get context :deletions)]
      (log/info "Committing deletion")
      (commit-work host-config deletion))
    ; commit work using :child
    ;(log/info "commit-root: " (pp context))
    (commit-work host-config (some-> context :wip-root-ref deref :child-ref))
    (swap! context-ref
      (fn [context]
        (assoc context
          :current-root-ref (get context :wip-root-ref)
          :wip-root-ref nil)))))

(defn reconcileChildren
  "wip-fiber is a zipper pointing to the current fiber"
  [context-ref wip-fiber-ref elements]
  {:pre [@context-ref]
   :post [@context-ref]}
  ; build a new wip-fiber while processing each of the elements,
  ; assign to context-ref when done
  (log/info "reconcileChildren elements\n" (pp elements))
  ;(log/info "reconcileChildren wip-fiber-ref\n" (pp wip-fiber-ref))
  (swap! context-ref
    (fn [context]
      (let [new-context
        (first
          (reduce
            (fn [[context index old-fiber-ref prev-sibling-ref] element]
              (assert (instance? Element element) element)
              (if (or
                    (< index (count elements))
                    @old-fiber-ref)
                (let [newFiber nil
                      #_ (log/info "old-fiber-ref" (pp old-fiber-ref))
                      #_ (log/info "element\n" (pp element))
                      sameType (and
                                 old-fiber-ref
                                 @old-fiber-ref
                                 element 
                                 (= (get element :type) (some-> old-fiber-ref deref :type)))
                      _ (log/info "same-type" sameType)
                      new-fiber-ref (cond
                                     sameType
                                       (new-fiber-ref
                                         (get @old-fiber-ref :type)
                                         (get element :props)
                                         (get @old-fiber-ref :dom)
                                         ; parent-ref
                                         wip-fiber-ref
                                         ; alternative
                                         old-fiber-ref
                                         "UPDATE")
                                      element
                                        (new-fiber-ref
                                          (get element :type)
                                          (get element :props)
                                          ; dom
                                          nil
                                          ; parent-ref
                                          wip-fiber-ref
                                          ; alternative
                                          nil
                                          "PLACEMENT")
                                      :else nil)
                      _        (when (and (some-> old-fiber-ref deref)
                                          (not sameType))
                                 (log/info "Logging DELETION")
                                 (swap! old-fiber-ref assoc :effectTag "DELETION"))
                       deletions (if (and old-fiber-ref
                                          @old-fiber-ref
                                          (not sameType))
                                   (conj (get context :deletions) old-fiber-ref)
                                   (get context :deletions))]
                       #_(log/info "reconcile-children new-fiber:\n" (-> new-fiber-ref deref pp))
                       (assert (:type @new-fiber-ref))
                       (cond
                         (zero? index)
                            (do (log/info "setting child-ref")
                            (swap! wip-fiber-ref assoc :child-ref new-fiber-ref))
                         element
                            (swap! prev-sibling-ref assoc :sibling-ref new-fiber-ref))
                        ;(log/info "reconcile-children loop context\n" (pp context))
                        (assert context)
                        ;(log/info "deletions" (pp deletions))
                        [(assoc context
                           :wip-fiber-ref wip-fiber-ref
                           :deletions deletions)
                         (inc index)
                         ; old-fiber ref
                         (some-> old-fiber-ref deref :sibling-ref)
                         ; new prev-sibling-ref is new-fiber-ref
                         new-fiber-ref])
                (do
                (assert context)
                [context index old-fiber-ref prev-sibling-ref])))
             [context
              0
              (some-> wip-fiber-ref deref :alternate-ref deref :child-ref)
              nil]
             elements))]
  (log/trace "reconcile-children new context\n" (pp new-context))
  (assert new-context)
  new-context))))

(def ^:dynamic *context-ref* nil)
(defn updateFunctionComponent
  [context-ref fiber-ref]
  {:post [@context-ref]}
  (binding [*context-ref* context-ref]
    (let [fiber @fiber-ref
          children [((get fiber :type) (get fiber :props))]]
      (swap! fiber-ref assoc :hooks [])
      (swap! context-ref
        assoc :wip-fiber-ref fiber-ref
              :hook-index 0)
      (reconcileChildren
        context-ref
        fiber-ref
        (or children [])))))

(defn updateHostComponent
  [host-config context-ref fiber-ref]
  {:pre [@context-ref]
   :post [@context-ref]}
  ;(log/info "update-host-component:\n" (-> fiber-ref deref pp))
  (when-not (-> fiber-ref deref :dom)
    (swap! fiber-ref
      (fn [fiber]
        (assoc fiber :dom
          (create-instance host-config
                       (:type fiber)
                       (:props fiber)
                       ; root-container-instance
                       nil
                       ; host-context
                       nil
                       ; internal-instance-handle
                       nil)))))
  (reconcileChildren
    context-ref
    fiber-ref
    (-> fiber-ref
      deref
      (get-in [:props :children] [])))
  (log/info "Done updating host componenet"))

(defn performUnitOfWork
  [host-config context-ref fiber-ref]
  ;(log/info "perform-unit-of-work working on fiber:\n" (-> fiber-ref deref pp))
  (let [isFunctionComponent (fn? (-> fiber-ref deref :type))]
    (if isFunctionComponent
      (updateFunctionComponent context-ref fiber-ref)
      (updateHostComponent host-config context-ref fiber-ref))
    (log/info "perform-unit-of-work calculating next fiberChild")
    (if-let [fiberChild (some-> fiber-ref deref :child-ref)]
      (do
        (log/info "perform-unit-of-work next fiber child is :child-ref")
        fiberChild)
      (loop [next-fiber-ref fiber-ref]
        #_(log/info "perform-unit-of-work looping fiber child" (pp next-fiber-ref))
        #_(log/info "perform-unit-of-work will recur " (some? (some-> next-fiber-ref deref)))
        #_(log/info "perform-unit-of-work will recur with " (some-> next-fiber-ref pp))
        (when (some-> next-fiber-ref deref)
          #_(log/info "perform-unit-of-work has sibling?" (some? (some-> next-fiber-ref deref :sibling-ref)))
          (if-let  [next-fiber-sibling-ref (some-> next-fiber-ref deref :sibling-ref)]
            next-fiber-sibling-ref 
            ; :parent
            (recur (some-> next-fiber-ref deref :parent-ref))))))))

#?(:clj
(defn start-work-loop!
  [context-ref host-config]
  (let [render-chan (chan)]
    (go-loop []
      (let [wip-root-ref (<! work-chan)]
        #_(log/info "next-unit-of-work\n" (pp wip-root-ref))
        ; push a new job onto the work-chan
        (swap! context-ref
          assoc
          :wip-root-ref wip-root-ref
          :deletions []
          :next-unit-of-work-ref wip-root-ref)
        (assert (some-> context-ref deref :wip-root-ref deref))
        (let [; FIXME use async timeouts
              timeRemaining 10
              next-unit-of-work-ref (loop [next-unit-of-work-ref (some-> context-ref deref :next-unit-of-work-ref)
                                         shouldYield false]
                                    (assert (some-> context-ref deref :wip-root-ref deref))
                                    (when  (and (some-> next-unit-of-work-ref deref)
                                              (not shouldYield))
                                      #_(log/info "performing unit of work\n" (pp next-unit-of-work-ref))
                                     
                                      (let [uow (performUnitOfWork
                                               host-config
                                               context-ref
                                               next-unit-of-work-ref)]
                                        (log/info "done with UoW")
                                        (recur uow (< timeRemaining 1)))))]
        ;(log/info "next-unit-of-work?: " (pp next-unit-of-work-ref))
        ;(log/info "context-ref" (pp @context-ref))
        ;(log/info "No more work" (not (some-> next-unit-of-work-ref deref)))
        ;(log/info "wip-root-ref" (some-> context-ref deref :wip-root-ref pp))
        (when (and  (not (some-> next-unit-of-work-ref deref))
                    (-> context-ref deref :wip-root-ref))
            (try
              (commitRoot host-config context-ref)
              (>!! render-chan true)
              (catch Throwable t
               (log/error t)))))
        (recur)))
    render-chan))
:cljs
(defn work-loop
  [context-ref host-config]
  (fn loop-fn [deadline]
    (loop [next-unit-of-work-ref (some-> context-ref deref :next-unit-of-work-ref)
           shouldYield false]
      (log/info "current UoW" (some-> next-unit-of-work-ref deref :type))
      ;(log/info "current UoW" (some-> next-unit-of-work-ref deref pp))
      (when  (and (some-> next-unit-of-work-ref deref)
                (not shouldYield))
        #_(log/info "performing unit of work\n" (pp next-unit-of-work-ref))
       
        (let [uow (performUnitOfWork
                 host-config
                 context-ref
                 next-unit-of-work-ref)]
          (log/info "done with UoW")
          (log/info "next UoW" (some-> uow deref :type))
          (swap! context-ref assoc :next-unit-of-work-ref uow)
          (recur uow (< (.timeRemaining deadline) 1)))))
    #_(log/info "Committing root?")
    (when (and  (not (some-> context-ref deref :next-unit-of-work-ref deref))
                (-> context-ref deref :wip-root-ref))
      #_(log/info "Committing root")
      (commitRoot host-config context-ref))
    (.requestIdleCallback js/window loop-fn))))

;(work-loop context-ref host-config)

; Hooks are stored in fibers under :hooks
(defrecord Hook [state queue])

(defn make-hook-ref [initial-state]
  (atom (Hook. initial-state [])))

; Public
(defn use-state
  [initial]
  ;; old hook?
  (let [context-ref *context-ref*
        context @context-ref
        old-hook-ref (some-> context :wip-fiber-ref deref :alternate-ref deref :hooks (nth (get context :hook-index)))
        hook-ref (make-hook-ref (if old-hook-ref
                                  (some-> old-hook-ref deref :state)
                                  initial))
        actions (if old-hook-ref
                  (some-> old-hook-ref deref :queue)
                  [])]
    (swap! hook-ref
      update :state (fn [state] (reduce (fn [state action] (action state)) state actions)))

    (log/info "use-state current-root-ref" (some? (:current-root-ref context-ref)))
    (let [set-state (fn set-state [action]
                      (log/info "got action" action)
                      ; deref from context-ref because context is no longer valid here
                      (swap! hook-ref update :queue conj action)
                      (swap! context-ref
                        (fn [context]
                          (let [current-root-ref (:current-root-ref context)
                                wip-root-ref (new-fiber-ref
                                               ; type
                                               nil
                                               ;props
                                               (get @current-root-ref :props)
                                               ;dom
                                               (get @current-root-ref :dom)
                                               ; parent-ref
                                               nil
                                               ; alternate-ref
                                               current-root-ref
                                               ; effectTag
                                               nil)]
                            (log/info "Adding next unit of work")
                            (assoc context
                              :wip-root-ref wip-root-ref
                              :next-unit-of-work-ref wip-root-ref
                              :deletions [])))))]
      (log/info "context has wip-fiber-ref?" (some? (:wip-fiber-ref context)))
      (swap! (-> context-ref deref :wip-fiber-ref) update :hooks conj hook-ref)
      (swap! context-ref update :hook-index inc)
      [(-> hook-ref deref :state) set-state])))


; const Didact = {
;   createElement,
;   render,
;   useState,
; }
