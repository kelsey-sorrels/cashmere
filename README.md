![cashmere logo](./resources/logo.png)

A Clojure wrapper for React. Runs React in GraalJS.

## Usage

```clojure
; Component are like functions which take props and context, and return elements
(require '[cashmere.core-graal :as g]')

(g/defcomponent Counter
  [props context]
  (let [[v set-v!] (g/use-state (get props :start))]
    (g/use-effect
      (fn []
        (future
          (doseq [i (range (inc (get props :start)) (get props :end))]
            ; Increase counter by one for each second that passes
            (Thread/sleep 1000)
            (set-v! (inc v)))))

      [])
    ; Components use Hiccup-syntax
    [:ul {:key "ul"}
     [:li {:key "li"} (str v)]]))

; Render Counter and listen for "DOM" updates
(let [render-chan (g/render-with-context Counter {:start 0 :end 10})]
  (go-loop []
    (let [container (<! render-chan)]
      ; print out DOM updates
      (println "container" (g/clj-elements container))
    (recur))))
```

## Wouldn't it be easier to do it another way?

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

Dependencies licensed and redistributed under their original respective licenses.
