(in-package :cosmic-lisp-explorer)

(defparameter *game-state* nil)
(defparameter *ui-mode* :terminal)

(defun initialize-game ()
  "Sets up the initial game state"
  (setf *game-state* (make-instance 'cosmic-lisp-explorer.game:game-state))
  (cosmic-lisp-explorer.game:generate-galaxy *game-state*)
  (cosmic-lisp-explorer.game:init-player *game-state*))

(defun game-loop ()
  "Main game loop"
  (let ((running t))
    (cosmic-lisp-explorer.ui:init-ui *ui-mode*)
    (unwind-protect
        (progn
          (cosmic-lisp-explorer.ui:render-game *game-state*)
          (loop while running do
            (let ((input (cosmic-lisp-explorer.ui:handle-input)))
              (case input
                (:quit (setf running nil))
                (:save (cosmic-lisp-explorer.utils:save-game *game-state* "save.dat"))
                (otherwise
                 (when (cosmic-lisp-explorer.game:process-turn *game-state* input)
                   (cosmic-lisp-explorer.ui:render-game *game-state*)))))))
      (cosmic-lisp-explorer.ui:shutdown-ui))))

(defun start-game (&optional (mode :terminal))
  "Start the game with the specified UI mode"
  (setf *ui-mode* mode)
  (initialize-game)
  (game-loop))

(defun main ()
  "Entry point for the standalone executable"
  (format t "Cosmic Lisp Explorer v0.1.0~%")
  (format t "Starting game in terminal mode...~%")
  (start-game :terminal)
  0) ; Return success code for process
