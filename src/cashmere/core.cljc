(ns cashmere.core
  (:require [cashmere.context :as gc]
            [cashmere.fiber :as gf]
            [cashmere.host-config :as ghc]
            [cashmere.template :as gt]
            [cashmere.begin-work :as gbw]
            [clojure.string]
            #?(:clj [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt! go go-loop]]
               :cljs [clojure.core.async :as async :refer [<! >! timeout chan alt! go go-loop]])
            [clojure.pprint]
            [taoensso.timbre :as log]))

(defn pp
  [x]
  (with-out-str (clojure.pprint/pprint x)))

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
  [host-config fiber-ref dom-parent]
  (if-let [dom (some-> fiber-ref deref :dom)]
    ;domParent.removeChild(fiber.dom)
    (ghc/remove-child-from-container host-config dom-parent dom)
    ; commit deletion of :child
    (commit-deletion host-config (some-> fiber-ref deref :child-ref) dom-parent)))

(defn cancel-effects
  [fiber-ref]
  (when-let [hooks  (some-> fiber-ref deref :hooks)]
    (doseq [effect-hook-ref hooks
            :let [effect-hook @effect-hook-ref]
            :when (and (= (:tag effect-hook) "effect")
                       (fn? (:cancel effect-hook)))]
      (log/trace "cancel-effects canceling" effect-hook)
      ((:cancel effect-hook)))))

(defn run-effects
  [fiber-ref]
  (log/trace "running effects for fiber"
    (pp (select-keys  @fiber-ref [:type :dom :hooks])))
  (when-let [hooks  (some-> fiber-ref deref :hooks)]
    (log/trace "got hooks" hooks)
    (doseq [effect-hook-ref hooks
            :let [effect-hook @effect-hook-ref]
            :when (and (= (:tag effect-hook) "effect")
                       (:effect effect-hook))]
      (log/trace "running effect" effect-hook)
      (swap! effect-hook-ref assoc :cancel ((:effect effect-hook))))))

(defn commit-work
  [host-config fiber-ref]
  ;(log/trace "commit-work" (some-> fiber-ref deref pp))
  (when (some-> fiber-ref deref)
    (log/trace "commit-work fiber-ref")
    (let [dom-parent-fiber-ref (loop [dom-parent-fiber-ref (:parent-ref @fiber-ref)]
                                 (if-not (some-> dom-parent-fiber-ref deref :dom)
                                   (recur (some-> dom-parent-fiber-ref deref :parent-ref))
                                   dom-parent-fiber-ref))
          dom-parent (-> dom-parent-fiber-ref deref :dom)]
      (log/trace "dom-parent" dom-parent)
      (log/trace "effectTag" (some-> fiber-ref deref :effectTag))
      (log/trace "dom?" (some-> fiber-ref deref :dom))
      ;(log/trace "fiber-ref\n" (pp fiber-ref))
      (let [recurse (cond
                      (= (some-> fiber-ref deref :effectTag) "PLACEMENT")
                        (do
                          (when-let [dom (some-> fiber-ref deref :dom)]
                            (log/info "host-config" host-config)
                            ; Place dom
                            (ghc/append-child-to-container
                              host-config
                              dom-parent
                              dom))
                          (run-effects fiber-ref)
                          true)
                      (= (some-> fiber-ref deref :effectTag) "UPDATE")
                        (do
                          (cancel-effects fiber-ref)
                          (when (some-> fiber-ref deref :dom)
                            (log/info "commit-update" 
                              (some-> fiber-ref deref :alternate-ref :props)
                              (some-> fiber-ref deref :props))
                            ; update dom
                            (ghc/commit-update
                              host-config
                              (some-> fiber-ref deref :dom)
                              nil
                              (some-> fiber-ref deref :type)
                              (some-> fiber-ref deref :alternate-ref :props)
                              (some-> fiber-ref deref :props)
                              ; finished work
                              nil))
                          (run-effects fiber-ref)
                          true)
                      (= (some-> fiber-ref deref :effectTag) "DELETION")
                        ; delete
                        (do
                          (cancel-effects fiber-ref)
                          (ghc/remove-child-from-container
                            host-config
                            (some-> fiber-ref deref :dom)
                            dom-parent)
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

;; put wip root fibers onto to initiate render
(defonce work-chan
  (chan))

; Public
(defn render
  [context-ref element container]
  ; create fiber for root
  (let [wip-root-ref (gf/new-fiber-ref
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
     (swap! context-ref
       assoc
       :wip-root-ref wip-root-ref
       :deletions []
       :next-unit-of-work-ref wip-root-ref)))

#?(:clj
  (defn render-sync
    [render-chan context-ref element container]
    (render context-ref element container)
    (<!! render-chan)))

(defn commit-root
  [host-config context-ref]
  (let [context @context-ref]
    (log/info "commit-root")
    ;(log/trace "commit-root deletions" (-> context :deletions pp))
    (doseq [deletion (get context :deletions)]
      (log/trace "Committing deletion")
      (commit-work host-config deletion))
    ; commit work using :child
    ;(log/trace "commit-root: " (pp context))
    (commit-work host-config (some-> context :wip-root-ref deref :child-ref))
    (ghc/reset-after-commit host-config (some-> context :wip-root-ref deref :dom))
    (log/info "setting wip-root-ref nil")
    (swap! context-ref
      (fn [context]
        (assoc context
          :current-root-ref (get context :wip-root-ref)
          :wip-root-ref nil)))))

(defn complete-work
  [host-config context-ref fiber-ref]
  (log/trace "complete-work calculating next fiberChild")
  ;(if-let [fiberChild (some-> fiber-ref deref :child-ref)]
  ;  (do
  ;    (log/trace "complete-work next fiber child is :child-ref")
  ;    fiberChild)
    (loop [next-fiber-ref fiber-ref i 0]
      #_(log/trace "perform-unit-of-work looping fiber child" (pp next-fiber-ref))
      #_(log/trace "perform-unit-of-work will recur " (some? (some-> next-fiber-ref deref)))
      #_(log/trace "perform-unit-of-work will recur with " (some-> next-fiber-ref pp))
      (when (and (< i 10) (some-> next-fiber-ref deref))
        #_(log/trace "perform-unit-of-work has sibling?" (some? (some-> next-fiber-ref deref :sibling-ref)))
        (if-let  [next-fiber-sibling-ref (some-> next-fiber-ref deref :sibling-ref)]
          next-fiber-sibling-ref 
          ; :parent
          (recur (some-> next-fiber-ref deref :parent-ref) (dec i))))))

(defn perform-unit-of-work
  [host-config context-ref unit-of-work]
  (if-let [next (gbw/begin-work host-config context-ref unit-of-work)]
    next
    (complete-work host-config context-ref unit-of-work)))

(defn work-loop-concurrent [host-config context-ref time-remaining-fn]
  (loop [next-unit-of-work-ref (some-> context-ref deref :next-unit-of-work-ref)
         should-yield false]
    ; TODO does this still hold true?
    ;(assert (some-> context-ref deref :wip-root-ref deref))
    (log/trace "work-loop-concurrent" (some-> next-unit-of-work-ref deref :type))
    (when  (and (some-> next-unit-of-work-ref deref)
                (not should-yield))
      #_(log/trace "performing unit of work\n" (pp next-unit-of-work-ref))
     
      (let [uow (perform-unit-of-work
                  host-config
                  context-ref
                  next-unit-of-work-ref)]
        (log/info "done with UoW. next UoW?" (some? uow))
        (swap! context-ref assoc :next-unit-of-work-ref uow)
        (recur uow (< (time-remaining-fn) 1))))))

#?(:clj
(defn start-work-loop!
  [context-ref host-config]
  (let [render-chan (chan)]
    (go-loop []
      (<! (timeout 1))
      (try
        (let [; FIXME use async timeouts
              time-remaining 10]
          (work-loop-concurrent host-config context-ref (constantly time-remaining)))
        (when (and  (not (some-> context-ref deref :next-unit-of-work-ref deref))
                    (-> context-ref deref :wip-root-ref))
              (log/info "committing root" (some-> context-ref deref :wip-root-ref deref :dom))
              (commit-root host-config context-ref)
              (log/info "after commit root"))
              ;(>!! render-chan true)
        (catch Throwable t
         (log/error t)))
      (recur))
    render-chan))
:cljs
(defn work-loop
  [context-ref host-config]
  (fn loop-fn [deadline]
    (letfn [(time-remaining-fn [] (.timeRemaining deadline))]
      (work-loop-concurrent host-config context-ref time-remaining-fn))
    #_(log/trace "Committing root?")
    (when (and  (not (some-> context-ref deref :next-unit-of-work-ref deref))
                (-> context-ref deref :wip-root-ref))
      #_(log/trace "Committing root")
      (commit-root host-config context-ref))
    (log/trace "Yielding to browser")
    (.requestIdleCallback js/window loop-fn))))

;(work-loop context-ref host-config)

; Hooks are stored in fibers under :hooks
(defrecord StateHook [state queue])
(defn make-state-hook-ref [initial-state]
  (atom (StateHook. initial-state [])))

; Public
(defn use-state
  [initial]
  ;; old hook?
  (let [context-ref gc/*context-ref*
        context @context-ref
        old-hook-ref (some-> context :wip-fiber-ref deref :alternate-ref deref :hooks (nth (get context :hook-index)))
        hook-ref (make-state-hook-ref (if old-hook-ref
                                        (some-> old-hook-ref deref :state)
                                        initial))
        actions (if old-hook-ref
                  (some-> old-hook-ref deref :queue)
                  [])]
    (swap! hook-ref
      update :state (fn [state] (reduce (fn [state action] (action state)) state actions)))

    (log/trace "use-state current-root-ref" (some? (:current-root-ref context-ref)))
    (log/trace "use-state hook-index" (get context :hook-index))
    (let [set-state (fn set-state [action]
                      (log/trace "got action" action)
                      ; deref from context-ref because context is no longer valid here
                      (swap! hook-ref update :queue conj action)
                      (swap! context-ref
                        (fn [context]
                          (let [current-root-ref (:current-root-ref context)
                                wip-root-ref (gf/new-fiber-ref
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
                            (log/trace "Adding next unit of work")
                            (assoc context
                              :wip-root-ref wip-root-ref
                              :next-unit-of-work-ref wip-root-ref
                              :deletions [])))))]
      (log/trace "context has wip-fiber-ref?" (some? (:wip-fiber-ref context)))
      (swap! (-> context-ref deref :wip-fiber-ref) update :hooks conj hook-ref)
      (swap! context-ref update :hook-index inc)
      [(-> hook-ref deref :state) set-state])))

(defn deps-changed?
  [prev-deps next-deps]
  (or (not prev-deps)
      (not next-deps)
      (not= prev-deps) next-deps))

(defrecord EffectHook [tag effect cancel deps])
(defn make-effect-hook-ref [tag effect cancel deps]
  (atom (EffectHook. tag effect cancel deps)))

(defn use-effect
  [effect deps]
  (let [context-ref gc/*context-ref*
        context @context-ref
        old-hook-ref (some-> context :wip-fiber-ref deref :alternate-ref deref :hooks (nth (get context :hook-index)))
        changed (deps-changed? (some-> old-hook-ref :deps) deps)
        hook-ref (make-effect-hook-ref
                   ; tag
                   "effect"
                   ; effect
                   (when changed effect)
                   ; cancel
                   (when changed
                     (some-> old-hook-ref deref :cancel))
                   ; deps
                   deps)]
      (log/trace "use-state hook-index" (get context :hook-index))
      (swap! (-> context-ref deref :wip-fiber-ref) update :hooks conj hook-ref)
      (swap! context-ref update :hook-index inc)))

(defn use-reducer
  [reducer initial-state]
  (let [[state set-state] (use-state initial-state)]
    (letfn [(dispatch [action]
              (let [next-state (reducer state action)]
                (set-state next-state)))]
      [state dispatch])))

; const Didact = {
;   createElement,
;   render,
;   useState,
; }
