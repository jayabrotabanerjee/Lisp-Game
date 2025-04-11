#!/usr/bin/env sbcl --script
#|
  Entry point script for running Cosmic Lisp Explorer with enhanced graphics
|#

(require :asdf)

;; Load Quicklisp if available
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the game system
(asdf:load-system :cosmic-lisp-explorer)

;; Start the game in enhanced graphics mode
(cosmic-lisp-explorer:start-game :enhanced)
