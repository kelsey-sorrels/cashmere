![gossamer logo](./resources/logo.png)

A Clojure React-like based on Rodrigo Pombo's Build Your Own React https://pomb.us/build-your-own-react/

## Usage

```clojure
; component are just functions with take props
(defn counter
  [props]
  (let [[state set-state!] (g/use-state 1)]
    ; Update the window title when the state changes
    (g/use-effect (fn [] (set! (.-title js/document) (str "from effect:"
state))) [state])
    ; Built in Hiccup support
    ; Increment state onClick
    [:h1 {:on-click (fn [] (set-state! (fn [c] (inc c))))}
      (str "Count: " state)]))

; Create dependencies
; Host config defines how the platform handels node changes
; Context contains global state
(let [host-config (g-dom/host-config)
      context-ref (g/new-context-ref)]
  ; Start reconciliation-loop
  (.requestIdleCallback js/window (g/work-loop context-ref host-config))
  ; Render the counter component into the "app" container
  (g/render
    context-ref
    ; Render the root component using Hiccup syntax
    [counter]
    (.getElementById js/document "app")))
```

## Why not use React?

React is great. Use it. If you just want Clojure, try this.

## Atoms everywhere? Yuck!

Fork it. Fix it.

## This doesn't support {probably some useful feature}!

Fork it. Fix it.

-License

Copyright © 2020 Aaron Santos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
