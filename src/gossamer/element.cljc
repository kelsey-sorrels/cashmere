(ns gossamer.element)

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


