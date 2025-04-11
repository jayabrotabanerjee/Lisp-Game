#!/usr/bin/env sbcl --script
#|
  Build script for creating a standalone executable of Cosmic Lisp Explorer
|#

(require :asdf)

;; Load Quicklisp if available
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the game system
(asdf:load-system :cosmic-lisp-explorer)

;; Ensure bin directory exists
(ensure-directories-exist "bin/")

;; Build the executable
(sb-ext:save-lisp-and-die
 "bin/cosmic-lisp-explorer"
 :toplevel #'cosmic-lisp-explorer:main
 :executable t
 :compression t)

(format t "~%Build complete! Executable created at bin/cosmic-lisp-explorer~%")
