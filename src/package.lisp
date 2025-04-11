(defpackage :cosmic-lisp-explorer
  (:use :cl :alexandria)
  (:export #:start-game
           #:main))

(defpackage :cosmic-lisp-explorer.game
  (:use :cl :alexandria)
  (:export #:make-world
           #:generate-galaxy
           #:player
           #:move-player
           #:game-state
           #:make-entity
           #:entity-at
           #:process-turn))

(defpackage :cosmic-lisp-explorer.ui
  (:use :cl :alexandria)
  (:export #:init-ui
           #:shutdown-ui
           #:render-game
           #:handle-input
           #:draw-map
           #:draw-ui))

(defpackage :cosmic-lisp-explorer.utils
  (:use :cl :alexandria)
  (:export #:perlin-noise
           #:cellular-automata
           #:random-between
           #:roll-dice
           #:save-game
           #:load-game))
