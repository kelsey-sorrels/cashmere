(ns gossamer.dom-host-config
  (:require [gossamer.core :as g]
            [taoensso.timbre :as log]))

(defn update-dom
  [dom prev-props next-props]
  (log/trace "update-dom" dom prev-props next-props)
  ;Remove old or changed event listeners
  (doseq [prop-name (->> prev-props
                      keys
                      (filter g/event?)
                      (filter (fn [k]
                                (or (not (contains? next-props k))
                                    (get (g/new? prev-props next-props) k)))))]
    (let [event-type (-> prop-name clojure.string/lower-case (subs 2))]
      (.removeEventListened dom event-type (get prev-props prop-name))))

  ; Remove old properties
  (doseq [prop-name (->> prev-props
                      keys
                      (filter g/property?)
                      (filter (g/gone? prev-props next-props)))]
    ; FIXME dom[name] = ""
    (aset dom prop-name ""))

  ; Set new or changed properties
  (doseq [prop-name (->> next-props
                      keys
                      (filter g/property?)
                      (filter (g/new? prev-props next-props)))]
    (aset dom prop-name (get next-props prop-name)))

  ; Add event listeners
  (doseq [prop-name (->> next-props
                      keys
                      (filter g/event?)
                      (filter (g/new? prev-props next-props)))]
    (let [event-type (-> prop-name clojure.string/lower-case (subs 2))]
      (.addEventListener dom event-type (get next-props prop-name))))
  dom)

(defrecord DomHostConfig []
  g/HostConfig
  (create-instance [this type props root-container-instance host-context internal-instance-handle]
    (log/trace "Creating node" type)
    (g/update-node this
      (if (= type ::g/TEXT_ELEMENT)
        (.createTextNode js/document "hello")
        (.createElement js/document (some-> type name)))
      {}
      props))
  (create-node [this node parent]
    (.appendChild parent node))
  (update-node [this node prev-props next-props]
    (update-dom node prev-props next-props))
  (delete-node [this node parent]
    (.removeChild parent node )))

(defn host-config
  []
  (DomHostConfig.))

