(ns gossamer.host-config)

(defprotocol HostConfig
  (get-public-instance [this])
  (get-root-host-context [this next-root-instance])
  (get-child-host-context [this parent-context fiber-type root-instance])
  (prepare-for-commit [this root-container-instance])
  (reset-after-commit [this root-container-instance])
  (create-instance [this type props root-container-instance host-context internal-instance-handle])
  (append-initial-child [this parent child])
  (finalize-initial-children [this instance type new-props root-container-instance current-host-context])
  (prepare-update [this instance type old-props new-props root-container-instance current-host-context])
  (should-set-text-content [this type next-props])
  (should-deprioritize-subtree [this])
  (create-text-instance [this new-text root-cotnainer-instance current-host-context work-in-progress])
  (schedule-timeout [this])
  (cancel-timeout [this])
  (no-timeout [this])
  (now [this])
  (is-primary-renderer [this])
  (warns-if-not-acting [this])
  (supports-mutation? [this])
  (supports-persistence? [this])
  (supports-hydration? [this])
  (get-fundamental-component-instance [this])
  (mount-fundamental-component [this])
  (should-update-fundamental-component [this])
  (get-instance-from-node [this])
  (before-remove-instance [this])
  (register-event [this])
  (mount-event-listener [this])
  (unmount-event-listener [this])
  (validate-event-listener-target [this])
  (is-opaque-hydrating-object [this])
  (make-opaque-hydrating-object [this])
  (make-client-id [this])
  (make-client-id-in-dev [this])
  (make-server-id [this]))

(def default-host-config {
  :get-public-instance (fn [this])
  :get-root-host-context (fn [this next-root-instance])
  :get-child-host-context (fn [this parent-context fiber-type root-instance])
  :prepare-for-commit (fn [this root-container-instance])
  :reset-after-commit (fn [this root-container-instance])
  :create-instance (fn [this type props root-container-instance host-context internal-instance-handle])
  :append-initial-child (fn [this parent child])
  :finalize-initial-children (fn [this instance type new-props root-container-instance current-host-context])
  :prepare-update (fn [this instance type old-props new-props root-container-instance current-host-context])
  :should-set-text-content (fn [this type next-props])
  :should-deprioritize-subtree (fn [this])
  :create-text-instance (fn [this new-text root-cotnainer-instance current-host-context work-in-progress])
  :schedule-timeout (fn [this])
  :cancel-timeout (fn [this])
  :no-timeout (fn [this])
  :now (fn [this])
  :is-primary-renderer (fn [this])
  :warns-if-not-acting (fn [this])
  :supports-mutation? (fn [this])
  :supports-persistence? (fn [this])
  :supports-hydration? (fn [this])
  :get-fundamental-component-instance (fn [this])
  :mount-fundamental-component (fn [this])
  :should-update-fundamental-component (fn [this])
  :get-instance-from-node (fn [this])
  :before-remove-instance (fn [this])
  :register-event (fn [this])
  :mount-event-listener (fn [this])
  :unmount-event-listener (fn [this])
  :validate-event-listener-target (fn [this])
  :is-opaque-hydrating-object (fn [this])
  :make-opaque-hydrating-object (fn [this])
  :make-client-id (fn [this])
  :make-client-id-in-dev (fn [this])
  :make-server-id (fn [this])})

(defprotocol MutationHostConfig
  (append-child [this parent-instance child])
  (append-child-to-container [this parent child])
  (commit-text-update [this text-instance old-text new-test])
  (commit-mount [this dom-element type new-props fiber-node])
  (commit-update [this instance update-payload type old-props new-props finished-work])
  (insert-before [this parent-instance child before-child])
  (insert-in-container-before [this child before-child])
  (remove-child [this parent-instance child])
  (remove-child-from-container [this container child])
  (reset-text-content [this dom-element])
  (hide-instance [this])
  (hide-text-instance [this])
  (unhide-instance [this])
  (unhide-text-instance [this])
  (update-fundamental-component [this])
  (unmount-fundamental-component [this]))

(def default-mutation-host-config {
  :append-child (fn [this parent-instance child])
  :append-child-to-container (fn [this parent child])
  :commit-text-update (fn [this text-instance old-text new-test])
  :commit-mount (fn [this dom-element type new-props fiber-node])
  :commit-update (fn [this instance update-payload type old-props new-props finished-work])
  :insert-before (fn [this parent-instance child before-child])
  :insert-in-container-before (fn [this child before-child])
  :remove-child (fn [this parent-instance child])
  :remove-child-from-container (fn [this container child])
  :reset-text-content (fn [this dom-element])
  :hide-instance (fn [this])
  :hide-text-instance (fn [this])
  :unhide-instance (fn [this])
  :unhide-text-instance (fn [this])
  :update-fundamental-component (fn [this])
  :unmount-fundamental-component (fn [this])})

(defprotocol PersistenceHostConfig
  (clone-instance [this current-instance update-payload type old-props new-props node children-unchanged recyclable-instance])
  (create-container-child-set [this container])
  (append-child-to-container-child-set [this container-child-set instance])
  ;; used in update-host-container
  (finalize-container-children [this container new-child-set])
  ;; Used in HostPortal
  (replace-container-children [this])
  (clone-hidden-instance [this])
  (clone-hidden-text-instance [this])
  (clone-fundamental-instance [this]))

(def default-persistence-host-config {
  :clone-instance (fn [this])
  :create-container-child-set (fn [this])
  :append-child-to-container-child-set (fn [this])
  :finalize-container-children (fn [this])
  :replace-container-children (fn [this])
  :clone-hidden-instance (fn [this])
  :clone-hidden-text-instance (fn [this])
  :clone-fundamental-instance (fn [this])})

