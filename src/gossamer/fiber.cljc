(ns gossamer.fiber
  (:require [clojure.pprint]))

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


