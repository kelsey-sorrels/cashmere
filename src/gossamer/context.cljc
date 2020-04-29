(ns gossamer.context)

(def ^:dynamic *context-ref* nil)

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

