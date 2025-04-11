(in-package :cosmic-lisp-explorer.ui)

(defvar *current-ui-mode* nil)

(defun init-ui (mode)
  "Initialize the UI based on the specified mode"
  (setf *current-ui-mode* mode)
  (case mode
    (:terminal (init-terminal-ui))
    (:enhanced (init-enhanced-ui))
    (otherwise (error "Unknown UI mode: ~A" mode))))

(defun shutdown-ui ()
  "Shut down the UI"
  (case *current-ui-mode*
    (:terminal (shutdown-terminal-ui))
    (:enhanced (shutdown-enhanced-ui))
    (otherwise (error "Unknown UI mode: ~A" *current-ui-mode*))))

(defun handle-input ()
  "Handle input based on the current UI mode"
  (case *current-ui-mode*
    (:terminal (handle-terminal-input))
    (:enhanced (handle-enhanced-input))
    (otherwise (error "Unknown UI mode: ~A" *current-ui-mode*))))

(defun render-game (game-state)
  "Render the game based on the current UI mode"
  (case *current-ui-mode*
    (:terminal (render-terminal-game game-state))
    (:enhanced (render-enhanced-game game-state))
    (otherwise (error "Unknown UI mode: ~A" *current-ui-mode*))))
