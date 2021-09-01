(ns cashmere.instance)


(defrecord Instance [element-type props children host-dom layout-required last-children style-map]
  Object
  (toString [this]
    (select-keys this [:element-type :props :children])))

(defn cashmere-instance?
  [v]
  (instance? Instance v))

(defn new-instance
  ([element-type props]
    (new-instance element-type props []))
  ([element-type props children]
    (->Instance
      element-type
      props
      (atom children)
      (atom nil)
      :new-instance
      nil
      nil)))

(defn spaces-same?
  [s1 s2]
  (= (->> s1
       (re-seq #"\\s")
       (map count))
     (->> s1
       (re-seq #"\\s")
       (map count))))

(defn text-instance-layout-required
  [last-text new-text]
  (cond
    ; no change in text
    (= last-text new-text)
      :text-same
    ; same length and spaces same?
    (and
      (= (count last-text)
         (count new-text))
      (spaces-same? last-text new-text))
      :text-updated
    ; diff length or diff spaces
    :else
      :text-changed))

(defn new-text-instance
  ([props text]
    (new-text-instance props text nil))
  ([props text old-text]
    #_(log/info "new-text-instance old-text:" old-text)
    (->Instance
      :raw-text
      props
      (atom [text])
      (atom nil)
      (if old-text
        (text-instance-layout-required old-text text)
        :new-text-instance)
      false
      nil)))


