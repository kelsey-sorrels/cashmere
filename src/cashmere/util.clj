(ns cashmere.util
  (:require [cashmere.instance :as ci]
            clojure.data
            clojure.string
            [clojure.zip :as zip]))
 


(defn tree-edges
  ([loc]
    (tree-edges loc true))
  ([loc rightmost]
    (if loc
      (let [has-right-sibling (some? (zip/right loc))]
        (conj (tree-edges (zip/up loc) false)
              (cond
                (and rightmost has-right-sibling)
                  "\u251c" ;├
                (and rightmost (not has-right-sibling))
                  "\u2514" ; └
                (and (not rightmost) has-right-sibling)
                  "\u2502" ; │
                (and (not rightmost) (not has-right-sibling))
                  " ")))

      [])))

(defn zipper-elements
  [root-element]
  (zip/zipper
    ; can have children
    (fn [v] (or (ci/cashmere-instance? v) (map? v) (map-entry? v)))
    ; children
    (fn [v]
      (cond
        (ci/cashmere-instance? v)
          @(:children v)
          #_(vec (array-map
            :props (nth v 1)
            :host-dom @(nth v 3)
            :children (nth v 2)))
        (map? v)
          (vec v)
        (map-entry? v)
          (if (coll? (second v))
            (second v)
            [(second v)])))
    ; with children
    (fn [v children]
      (cond
        (ci/cashmere-instance? v)
          (assoc v :children children)
          #_(let [[tag props _ host-dom] v]
            [tag props children host-dom])
        (map? v)
          {(ffirst v) children}
        (map-entry? v)
          (first {(first v) children})))
    root-element))


(defn zipper-descendants
  [z]
  #_(log/info "zipper-descendants" z)
  (if-not (zip/end? z)
    (lazy-seq
      (cons z (zipper-descendants (zip/next z))))
    []))

(defn tree->str [root-element]
  (clojure.string/join "\n"
    (cons "\nroot-element"
      (for [z (-> root-element zipper-elements zipper-descendants)
            :let [node (zip/node z)]]
        (str (clojure.string/join (tree-edges z))
             (cond
               (ci/cashmere-instance? node)
                 (let [attrs [["key" (some-> node :props :key)]
                              ["host-dom" (some-> node :host-dom deref)]
                              #_["keys" (some-> node keys)]]]
				   (str
                      (:element-type node)
                      (when (some second attrs)
                        (str " ("
                           (clojure.string/join " "
                              (map (fn [[k v]] (str k "=" v)) attrs))
                           ")"))))
               (map-entry? node)
                 (first node)
               (map? node)
                 {}
               (nil? node)
                 "nil"
               :else
                 node))))))

