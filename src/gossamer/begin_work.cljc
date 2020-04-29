(ns gossamer.begin-work
  (:require [gossamer.context :as gc]
            [gossamer.fiber :as gf]
            [gossamer.host-config :as ghc]
            [gossamer.template :as gt]
            [clojure.pprint]
            [taoensso.timbre :as log]))

(defn pp
  [x]
  (with-out-str (clojure.pprint/pprint x)))

(defn reconcile-children
  "wip-fiber is a zipper pointing to the current fiber"
  [context-ref wip-fiber-ref elements]
  {:pre [@context-ref]
   :post [@context-ref]}
  ; build a new wip-fiber while processing each of the elements,
  ; assign to context-ref when done
  (log/info "reconcile-children elements\n" (pp elements))
  ;(log/trace "reconcile-children wip-fiber-ref\n" (pp wip-fiber-ref))
  (swap! context-ref
    (fn [context]
      (let [new-context
        (first
          (reduce
            (fn [[context index old-fiber-ref prev-sibling-ref] element]
              #_(assert (instance? Element element) element)
              (if (or
                    (< index (count elements))
                    @old-fiber-ref)
                (let [newFiber nil
                      _ (log/trace "old-fiber-ref" (pp old-fiber-ref))
                      _ (log/trace "element\n" (pp element))
                      element (gt/as-element element)
                      sameType (and
                                 old-fiber-ref
                                 @old-fiber-ref
                                 element
                                 (= (get element :type) (some-> old-fiber-ref deref :type)))
                      _ (log/info "same-type" sameType)
                      _ (log/info "element" element)
                      new-fiber-ref (cond
                                     sameType
                                       (gf/new-fiber-ref
                                         (get @old-fiber-ref :type)
                                         (get element :props)
                                         (get @old-fiber-ref :dom)
                                         ; parent-ref
                                         wip-fiber-ref
                                         ; alternative
                                         old-fiber-ref
                                         "UPDATE")
                                       element
                                        (gf/new-fiber-ref
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
                                 (log/trace "Logging DELETION")
                                 (swap! old-fiber-ref assoc :effectTag "DELETION"))
                       deletions (if (and old-fiber-ref
                                          @old-fiber-ref
                                          (not sameType))
                                   (conj (get context :deletions) old-fiber-ref)
                                   (get context :deletions))]
                       #_(log/trace "reconcile-children new-fiber:\n" (-> new-fiber-ref deref pp))
                       (assert (:type @new-fiber-ref))
                       (cond
                         (zero? index)
                            (do (log/info "setting child-ref")
                            (swap! wip-fiber-ref assoc :child-ref new-fiber-ref))
                         element
                            (swap! prev-sibling-ref assoc :sibling-ref new-fiber-ref))
                        ;(log/trace "reconcile-children loop context\n" (pp context))
                        (assert context)
                        ;(log/trace "deletions" (pp deletions))
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

(defn host-config-type
  [host-config]
  (cond
    (satisfies? ghc/MutationHostConfig host-config) ::mutation
    (satisfies? ghc/PersistenceHostConfig host-config) ::persistence
    :else :default))

(defn update-function-component
  [context-ref fiber-ref]
  {:post [@context-ref]}
  (binding [gc/*context-ref* context-ref]
    (swap! context-ref
      assoc :wip-fiber-ref fiber-ref
            :hook-index 0)
    (swap! fiber-ref assoc :hooks [])
    (let [fiber @fiber-ref
          children [((get fiber :type) (get fiber :props))]]
      (reconcile-children
        context-ref
        fiber-ref
        (or children []))))
  (some-> fiber-ref deref :child-ref))

(defmulti update-host-root (fn [host-config _ _] (host-config-type host-config)))

(defmethod update-host-root ::mutation
  [host-config context-ref fiber-ref]
  ;; noop
  (some-> context-ref deref :wip-fiber-ref deref :child-ref))

(defn append-all-children-to-container
  [host-config container-child-set fiber-ref]
  (let [node (some-> fiber-ref deref :child-ref)]
    (loop [node node]
      (when node
          (let [instance (some-> node deref :dom)]
            (ghc/append-child-to-container-child-set host-config container-child-set instance))
          (some-> node deref :child-ref deref)
            (recur (some-> node deref :child-ref))))))

(defmethod update-host-root ::persistence
  [host-config context-ref fiber-ref]
  {:pre [@context-ref]
   :post [@context-ref]}
  (log/info "update-host-root")
  #_(when-not (-> fiber-ref deref :dom)
      (swap! fiber-ref
        (fn [fiber]
          ; FIXME what container?
          (let [container (:dom fiber)
            new-child-set (ghc/create-container-child-set host-config container)]
            (append-all-children-to-container host-config new-child-set fiber-ref)
            ;(.-pending-children new-child-set)
            (ghc/finalize-container-children host-config container new-child-set)
          (assoc fiber :dom
            (ghc/create-instance
               host-config
               (:type fiber)
               (:props fiber)
               ; root-container-instance
               (some-> context-ref deref :current-root-ref deref :dom)
               ; host-context
               nil
               ; internal-instance-handle
               nil)))))
    ; FIXME should this be commented out?
    (reconcile-children
      context-ref
      fiber-ref
      (-> fiber-ref
        deref
        (get-in [:props :children] []))))
    (log/trace "Done updating host container")
  nil
  #_(some-> context-ref deref :wip-fiber-ref deref :child-ref))
 

(defmulti update-host-component (fn [host-config _ _] (host-config-type host-config)))

(defmethod update-host-component ::mutation
  [host-config context-ref fiber-ref]
  {:pre [@context-ref]
   :post [@context-ref]}
  ;(log/trace "update-host-component:\n" (-> fiber-ref deref pp))
  (log/info "update-host-component" host-config)
  (when-not (-> fiber-ref deref :dom)
    (swap! fiber-ref
      (fn [fiber]
        (assoc fiber :dom
          (ghc/create-instance
             host-config
             (:type fiber)
             (:props fiber)
             ; root-container-instance
             nil
             ; host-context
             nil
             ; internal-instance-handle
             nil)))))
  (reconcile-children
    context-ref
    fiber-ref
    (-> fiber-ref
      deref
      (get-in [:props :children] [])))
  (log/trace "Done updating host componenet")
  (some-> fiber-ref deref :child-ref))

(defmethod update-host-component ::persistence
  [host-config context-ref fiber-ref]
  (let [type (some-> fiber-ref deref :type)
        old-props (some-> fiber-ref deref :alternate-ref :props)
        new-props (some-> fiber-ref deref :props)
        new-instance (ghc/clone-instance
                       host-config
                       ;currentInstance
                       (some-> fiber-ref deref :dom)
                       ;updatePayload
                       nil
                       type
                       old-props
                       new-props
                       fiber-ref
                       ;childrenUnchanged
                       nil
                       ;recyclableInstance
                       nil)]
    ; FIXME Should this be here?
    (swap! fiber-ref
      (fn [fiber]
        (assoc fiber :dom new-instance)))
    ; FIXME Should this be here?
    (reconcile-children
      context-ref
      fiber-ref
      (-> fiber-ref
        deref
        (get-in [:props :children] [])))
    #_(ghc/finalize-initial-children
      host-config
      new-instance
      type
      new-props
      ; root-container-instance
      (some-> context-ref deref :current-root-ref deref :dom)
      ;currentHostContext
      nil))
  (some-> fiber-ref deref :child-ref))

(defn begin-work
  [host-config context-ref fiber-ref]
  ;(log/trace "perform-unit-of-work working on fiber:\n" (-> fiber-ref deref pp))
  (let [isFunctionComponent (fn? (-> fiber-ref deref :type))
        ; Root fiber was created using a :type of nil, so check for that
        isHostRoot (nil? (-> fiber-ref deref :type))]
    (log/info "begin-work" (-> fiber-ref deref :type) (-> fiber-ref deref :props) isFunctionComponent isHostRoot)
    (cond
      isFunctionComponent
        (update-function-component context-ref fiber-ref)
      isHostRoot
        (update-host-root host-config context-ref fiber-ref)
      :else
        (update-host-component host-config context-ref fiber-ref))))

