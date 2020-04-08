# gossamer

A Clojure React-like based on Rodrigo Pombo's Build Your Own React https://pomb.us/build-your-own-react/

## Usage

```clojure
; create a host config - this defines how host platform works
(def host-config (dom-host-config))

; create a context reference - this keeps the render state
(def context-ref (new-context-ref)

; start a render loop - this runs the reconciler in the background
(def render-loop (work-loop context-ref host-config))

; component are just functions with take props
(defn counter
  [props]
  (let [[state set-state!] (use-state 1)]
    (create-element :counter {}
      (str "count: " state))))

; render components (sync blocks until rendering is complete)
(render-sync
   render-chan
   context-ref
   ; the component to render
   (create-element counter {})
   ; the container
   nil)
```

## Why not use React?

React is great. Use it. If you just want Clojure, try this.

## Refs everywhere? Yuck!

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
