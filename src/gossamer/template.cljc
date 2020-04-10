(ns gossamer.template
  (:require [gossamer.element :as ge]
            [clojure.string]
            [clojure.walk :refer [prewalk]]
            [taoensso.timbre :as log]))

; From reagent.impl.util
(def dont-camel-case #{"aria" "data"})

(defn capitalize [s]
  (if (< (count s) 2)
    (clojure.string/upper-case s)
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(defn dash-to-prop-name [dashed]
  (if (string? dashed)
    dashed
    (let [name-str (name dashed)
          [start & parts] (clojure.string/split name-str #"-")]
      (if (dont-camel-case start)
        name-str
        (apply str start (map capitalize parts))))))

(defn dash-to-method-name [dashed]
  (if (string? dashed)
    dashed
    (let [name-str (name dashed)
          name-str (clojure.string/replace name-str #"(unsafe|UNSAFE)[-_]" "UNSAFE_")
          [start & parts] (clojure.string/split name-str #"-")]
      (apply str start (map capitalize parts)))))

(defn fun-name [f]
  (let [n (or (and (fn? f)
                   (or (.-displayName f)
                       (let [n (.-name f)]
                         (if (and (string? n) (seq n))
                           n))))
              (and (satisfies? clojure.lang.Named f)
                   (name f))
              (let [m (meta f)]
                (if (map? m)
                  (:name m))))]
    (if n
      (clojure.string/replace (str n) "$" "."))))

(defn named? [x]
  (or (keyword? x)
      (symbol? x)))

(defn class-names
  ([])
  ([class]
   (if (coll? class)
     (let [classes (keep (fn [c]
                           (if c
                             (if (named? c)
                               (name c)
                               c)))
                         class)]
       (if (seq classes)
         (clojure.string/join " " classes)))
     (if (named? class)
       (name class)
       class)))
  ([a b]
   (if a
     (if b
       (str (class-names a) " " (class-names b))
       (class-names a))
     (class-names b)))
  ([a b & rst]
   (reduce class-names
           (class-names a b)
           rst)))

(def prop-name-cache {:class "className"
                      :for "htmlFor"
                      :charset "charSet"})

(declare as-element)

;; From Weavejester's Hiccup, via pump:
(def ^{:doc "Regular expression that parses a CSS-style id and class
             from a tag name."}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(deftype NativeWrapper [tag id className])


;;; Common utilities

(defn hiccup-tag? [x]
  (or (named? x)
      (string? x)))

(defn valid-tag? [x]
  (or (hiccup-tag? x)
      (ifn? x)
      (instance? NativeWrapper x)))

;;; Props conversion

(defn cache-get [o k]
  (when ^boolean (.hasOwnProperty o k)
    ; FIXME use cache
    #_(gobj/get o k)))

(defn cached-prop-name [k]
  (if (named? k)
    (if-some [k' (cache-get prop-name-cache (name k))]
      k'
      (let [v (dash-to-prop-name k)]
        ; FIXME use cache
        ;(gobj/set prop-name-cache (name k) v)
        v))
    k))

(declare convert-prop-value)

; FIXME Cache
(defn kv-conv [o k v]
  (merge o {(cached-prop-name k) (convert-prop-value v)}))

(defn convert-prop-value [x]
  ; FIXME handle cases
  (cond #_#_(js-val? x) x
        (named? x) (name x)
        (map? x) (reduce-kv kv-conv {} x)
        (coll? x) x #_(clj->js x)
        (ifn? x) (fn [& args]
                   (apply x args))
        :else x #_(clj->js x)))


(defn custom-kv-conv [o k v]
  ; FIXME cache
  #_(doto o
    (gobj/set (cached-custom-prop-name k) (convert-prop-value v))))

(defn convert-custom-prop-value [x]
  ; FIXME handle cases
  (cond #_#_(js-val? x) x
        (named? x) (name x)
        (map? x) (reduce-kv custom-kv-conv {} x)
        (coll? x) x #_(clj->js x)
        (ifn? x) (fn [& args]
                   (apply x args))
        :else x #_(clj->js x)))

(defn set-id-class
  "Takes the id and class from tag keyword, and adds them to the
  other props. Parsed tag is JS object with :id and :class properties."
  [props id-class]
  (let [id (.-id id-class)
        class (.-className id-class)]
    (cond-> props
      ;; Only use ID from tag keyword if no :id in props already
      (and (some? id)
           (nil? (:id props)))
      (assoc :id id)

      ;; Merge classes
      class
      (assoc :class (class-names class (:class props))))))

(defn convert-props [props id-class]
  (let [class (:class props)
        props (-> props
                  (cond-> class (assoc :class (class-names class)))
                  (set-id-class id-class))]
    (if (.-custom id-class)
      (convert-custom-prop-value props)
      (convert-prop-value props))))

(declare make-element)

;;; Conversion from Hiccup forms

(defrecord HiccupTag [tag id className custom])

(defn comp-name
 []
 ;  FIXME if dev then use *current-component*
 "")

(defn parse-tag [hiccup-tag]
  (let [[tag id className] (->> hiccup-tag name (re-matches re-tag) next)
        className (when-not (nil? className)
                    (clojure.string/replace className #"\." " "))]
    (assert tag (str "Invalid tag: '" hiccup-tag "'" (comp-name)))
    (->HiccupTag tag
                 id
                 className
                 ;; Custom element names must contain hyphen
                 ;; https://www.w3.org/TR/custom-elements/#custom-elements-core-concepts
                 (not= -1 (.indexOf tag "-")))))

(defn try-get-key [x]
  ;; try catch to avoid clojurescript peculiarity with
  ;; sorted-maps with keys that are numbers
  (try (get x :key)
       (catch java.lang.Throwable e)))

(defn get-key [x]
  (when (map? x)
    (try-get-key x)))

(defn key-from-vec [v]
  (if-some [k (-> (meta v) get-key)]
    k
    (-> v (nth 1 nil) get-key)))

; FIXME hack
(defn as-class [tag]
  tag)

(defn reag-element [tag v]
  (let [c (as-class tag)
        jsprops {}]
    (set! (.-argv jsprops) v)
    (when-some [key (key-from-vec v)]
      (set! (.-key jsprops) key))
    ; FIXME - do the thing
    (ge/create-element c jsprops)))

(defn fragment-element [argv]
  (let [props (nth argv 1 nil)
        hasprops (or (nil? props) (map? props))
        jsprops (or (convert-prop-value (if hasprops props))
                    {})
        first-child (+ 1 (if hasprops 1 0))]
    (when-some [key (key-from-vec argv)]
      (set! (.-key jsprops) key))
    ; FIXME remove nil, use react/Fragment equivalent
    (make-element argv nil #_react/Fragment jsprops first-child)))

(defn adapt-react-class
  [c]
  (->NativeWrapper c nil nil))

(def tag-name-cache {})

(defn cached-parse [x]
  (if-some [s (cache-get tag-name-cache x)]
    s
    (let [v (parse-tag x)]
      ; FIXME use cache?
      ;(gobj/set tag-name-cache x v)
      v)))

(defn native-element [parsed argv first]
  (let [component (:tag parsed)
        props (nth argv first nil)
        hasprops (or (nil? props) (map? props))
        jsprops (or (convert-props (if hasprops props) parsed)
                    {})
        first-child (+ first (if hasprops 1 0))]
    ;(if (input-component? component)
    ;  (-> [(reagent-input) argv component jsprops first-child]
    ;      (with-meta (meta argv))
    ;      as-element)
      (do
        (when-some [key (-> (meta argv) get-key)]
          (set! (.-key jsprops) key))
        (make-element argv component jsprops first-child))))
;)

(defn dev? []
  false)

(defn str-coll [coll]
  (if (dev?)
    (str (prewalk (fn [x]
                    (if (fn? x)
                      (let [n (fun-name x)]
                        (case n
                          ("" nil) x
                          (symbol n)))
                      x)) coll))
    (str coll)))

(defn hiccup-err [v & msg]
  (str (apply str msg) ": " (str-coll v) "\n" (comp-name)))

; FIXME where is this coming from? Hacked it in
#?(:clj
(defn keyword-identical?
  [kw1 kw2]
  (= kw1 kw2)))

(defn vec-to-elem [v]
  (assert (pos? (count v)) (hiccup-err v "Hiccup form should not be empty"))
  (let [tag (nth v 0 nil)]
    (assert (valid-tag? tag) (hiccup-err v "Invalid Hiccup form"))
    (cond
      (keyword-identical? :<> tag)
      (fragment-element v)

      (hiccup-tag? tag)
      (let [n (name tag)
            pos (.indexOf n ">")]
        (case pos
          -1 (native-element (cached-parse n) v 1)
          0 (let [component (nth v 1 nil)]
              ;; Support [:> component ...]
              (assert (= ">" n) (hiccup-err v "Invalid Hiccup tag"))
              (native-element (->HiccupTag component nil nil nil) v 2))
          ;; Support extended hiccup syntax, i.e :div.bar>a.foo
          ;; Apply metadata (e.g. :key) to the outermost element.
          ;; Metadata is probably used only with sequeneces, and in that case
          ;; only the key of the outermost element matters.
          (recur (with-meta [(subs n 0 pos)
                             (assoc (with-meta v nil) 0 (subs n (inc pos)))]
                            (meta v)))))

      (instance? NativeWrapper tag)
      (native-element tag v 1)

      :else (reag-element tag v))))

(declare expand-seq)
#_(declare expand-seq-dev)

(defn as-element [x]
  ; FIXME js
  (cond #_#_(js-val? x) x
        (vector? x) (vec-to-elem x)
        (seq? x) ;(if (dev?)
                 ;  (expand-seq-dev x)
                   (expand-seq x)
        (named? x) (name x)
        #_#_(satisfies? IPrintWithWriter x) (pr-str x)
        :else x))

(defn expand-seq [s]
  (into-array (map as-element s)))

#_(defn expand-seq-dev [s ^clj o]
  (into-array (map (fn [val]
                     (when (and (vector? val)
                                (nil? (key-from-vec val)))
                       (set! (.-no-key o) true))
                     (as-element val))
                   s)))

#_(defn expand-seq-check [x]
  (let [ctx {}
        [res derefed] (ratom/check-derefs #(expand-seq-dev x ctx))]
    (when derefed
      (warn (hiccup-err x "Reactive deref not supported in lazy seq, "
                        "it should be wrapped in doall")))
    (when (.-no-key ctx)
      (warn (hiccup-err x "Every element in a seq should have a unique :key")))
    res))

(defn make-element [argv component jsprops first-child]
  (case (- (count argv) first-child)
    ;; Optimize cases of zero or one child
    0 (ge/create-element component jsprops)

    1 (ge/create-element component jsprops
          (as-element (nth argv first-child nil)))

    (apply ge/create-element
            (reduce-kv (fn [a k v]
                         (when (>= k first-child)
                           (conj a (as-element v)))
                         a)
                       [component jsprops] argv))))
