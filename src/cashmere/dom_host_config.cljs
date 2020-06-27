(ns cashmere.dom-host-config
  (:require [cashmere.core :as g]
            [cashmere.element :as ge]
            [cashmere.host-config :as ghc]
            [taoensso.timbre :as log]))

(defn update-dom
  [dom prev-props next-props]
  (log/info "update-dom" dom prev-props next-props)
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
  ghc/HostConfig
  (get-public-instance [this] nil)
  (get-root-host-context [this next-root-instance] nil)
  (get-child-host-context [this parent-context fiber-type root-instance] nil)
  (prepare-for-commit [this root-container-instance] nil)
  (reset-after-commit [this root-container-instance] nil)
  (create-instance [this type props root-container-instance host-context internal-instance-handle]
    (log/trace "Creating node" type)
    (update-dom
      (if (= type ::ge/TEXT_ELEMENT)
        (.createTextNode js/document "hello")
        (.createElement js/document (some-> type name)))
      {}
      props))
  (append-initial-child [this parent child] nil)
  (finalize-initial-children [this instance type new-props root-container-instance current-host-context] nil)
  (prepare-update [this instance type old-props new-props root-container-instance current-host-context] nil)
  (should-set-text-content [this type next-props] nil)
  (should-deprioritize-subtree [this] nil)
  ; TODO use
  (create-text-instance [this new-text root-cotnainer-instance current-host-context work-in-progress]
    (.createTextNode js/document new-text))
  (schedule-timeout [this] nil)
  (cancel-timeout [this] nil)
  (no-timeout [this] nil)
  (now [this] nil)
  (is-primary-renderer [this] nil)
  (warns-if-not-acting [this] nil)
  (supports-mutation? [this] nil)
  (supports-persistence? [this] nil)
  (supports-hydration? [this] nil)
  (get-fundamental-component-instance [this] nil)
  (mount-fundamental-component [this] nil)
  (should-update-fundamental-component [this] nil)
  (get-instance-from-node [this] nil)
  (before-remove-instance [this] nil)
  (register-event [this] nil)
  (mount-event-listener [this] nil)
  (unmount-event-listener [this] nil)
  (validate-event-listener-target [this] nil)
  (is-opaque-hydrating-object [this] nil)
  (make-opaque-hydrating-object [this] nil)
  (make-client-id [this] nil)
  (make-client-id-in-dev [this] nil)
  (make-server-id [this] nil)

  ghc/MutationHostConfig
  (append-child [this parent-instance child] nil)
  (append-child-to-container [this parent child]
    (.appendChild parent child))
  (commit-text-update [this text-instance old-text new-test] nil)
  (commit-mount [this dom-element type new-props fiber-node] nil)
  (commit-update [this instance update-payload type old-props new-props finished-work]
    (update-dom instance old-props new-props))
  (insert-before [this parent-instance child before-child] nil)
  (insert-in-container-before [this child before-child] nil)
  (remove-child [this parent-instance child] nil)
  (remove-child-from-container [this node parent]
    (.removeChild parent node))
  (reset-text-content [this dom-element] nil)
  (hide-instance [this] nil)
  (hide-text-instance [this] nil)
  (unhide-instance [this] nil)
  (unhide-text-instance [this] nil)
  (update-fundamental-component [this] nil)
  (unmount-fundamental-component [this] nil))

(defn host-config
  []
  (DomHostConfig.))

