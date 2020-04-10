(ns gossamer.corejs
  (:require [gossamer.core :as g]
            [gossamer.element :as ge]
            [gossamer.dom-host-config :as g-dom]
            [taoensso.timbre :as log]))

(enable-console-print!)

(println "This text is printed from src/gossamer/core.cljs. Go ahead and edit it and see reloading in action.")

(defn counter
  [props]
  (let [[state set-state!] (g/use-state 1)]
    (g/use-effect (fn [] (set! (.-title js/document) (str "from effect:" state))) [state])
    [:h1 {:on-click (fn [] (set-state! (fn [c] (inc c))))}
      (str "Count: " state)]))

(let [host-config (g-dom/host-config)
	  context-ref (g/new-context-ref)
	  render-chan (g/work-loop
					context-ref
					host-config)]
  ; start render-loop
  (.requestIdleCallback js/window (g/work-loop context-ref host-config))
  (g/render
	context-ref
	[counter]
	(.getElementById js/document "app")))

(defn on-js-reload []
  (log/info "on-js-reload"))
