(in-package :cosmic-lisp-explorer.ui)

(defparameter *screen* nil)
(defparameter *colors-initialized* nil)

(defun init-terminal-ui ()
  "Initialize the terminal UI using cl-charms"
  (setf *screen* (charms:initialize))
  (charms:disable-echoing)
  (charms:enable-raw-input)
  (charms:enable-non-blocking-mode *screen*)
  
  ;; Initialize colors if the terminal supports them
  (when (charms:has-colors)
    (unless *colors-initialized*
      (charms:start-color)
      ;; Define color pairs
      (charms:init-pair 1 charms:color_white charms:color_black)     ; Default
      (charms:init-pair 2 charms:color_yellow charms:color_black)    ; Star
      (charms:init-pair 3 charms:color_green charms:color_black)     ; Earth-like
      (charms:init-pair 4 charms:color_blue charms:color_black)      ; Ice
      (charms:init-pair 5 charms:color_red charms:color_black)       ; Rocky
      (charms:init-pair 6 charms:color_magenta charms:color_black)   ; Gas giant
      (charms:init-pair 7 charms:color_cyan charms:color_black)      ; Station
      (setf *colors-initialized* t))))

(defun shutdown-terminal-ui ()
  "Shut down the terminal UI"
  (charms:disable-raw-input)
  (charms:disable-non-blocking-mode *screen*)
  (charms:end-screen))

(defun handle-terminal-input ()
  "Handle keyboard input in terminal mode"
  (let ((c (charms:get-char *screen* :ignore-error t)))
    (case c
      ((#\q #\Q) :quit)
      ((#\w #\W #\8) :up)
      ((#\s #\S #\2) :down)
      ((#\a #\A #\4) :left)
      ((#\d #\D #\6) :right)
      (#\space :interact)
      ((#\i #\I) :inventory)
      ((#\m #\M) :map)
      ((#\e #\E) :equipment)
      ((#\v #\V) :save)
      (otherwise nil))))

(defun render-terminal-game (game-state)
  "Render the game in terminal mode"
  (charms:clear-window *screen*)
  (charms:refresh-window *screen*)
  
  ;; Get terminal dimensions
  (multiple-value-bind (max-y max-x) (charms:window-dimensions *screen*)
    ;; Draw the header
    (charms:write-string-at-point *screen* "COSMIC LISP EXPLORER" 2 0)
    (let ((turn-str (format nil "Turn: ~A" (cosmic-lisp-explorer.game:turn-count game-state))))
      (charms:write-string-at-point *screen* turn-str (- max-x (length turn-str) 2) 0))
    
    (charms:write-string-at-point *screen* 
                                 (make-string (- max-x 2) :initial-element #\-) 
                                 1 1)
    
    ;; Draw the current system
    (let ((current-system (cosmic-lisp-explorer.game:current-system game-state)))
      (when current-system
        (charms:write-string-at-point *screen* 
                                     (format nil "Star System: ~A" 
                                             (cosmic-lisp-explorer.game:name current-system))
                                     2 2)
        
        ;; Draw the map
        (draw-terminal-map game-state max-x max-y)
        
        ;; Draw player stats
        (draw-terminal-player-stats game-state max-x max-y)
        
        ;; Draw the log messages
        (draw-terminal-log game-state max-x max-y)))
    
    (charms:refresh-window *screen*)))

(defun draw-terminal-map (game-state max-x max-y)
  "Draw the system map on the terminal"
  (let* ((player (cosmic-lisp-explorer.game:player game-state))
         (current-system (cosmic-lisp-explorer.game:current-system game-state))
         (map-width 40)
         (map-height 20)
         (map-x 2)
         (map-y 4)
         (center-x (cosmic-lisp-explorer.game:x player))
         (center-y (cosmic-lisp-explorer.game:y player))
         (start-x (- center-x (floor map-width 2)))
         (start-y (- center-y (floor map-height 2))))
    
    ;; Draw border
    (dotimes (x (+ map-width 2))
      (charms:write-char-at-point *screen* #\- (+ map-x x) map-y)
      (charms:write-char-at-point *screen* #\- (+ map-x x) (+ map-y map-height 1)))
    
    (dotimes (y (+ map-height 2))
      (charms:write-char-at-point *screen* #\| map-x (+ map-y y))
      (charms:write-char-at-point *screen* #\| (+ map-x map-width 1) (+ map-y y)))
    
    ;; Draw the map contents
    (dotimes (y map-height)
      (dotimes (x map-width)
        (let ((world-x (+ start-x x))
              (world-y (+ start-y y)))
          (when (and (>= world-x 0) (>= world-y 0)
                     (< world-x (cosmic-lisp-explorer.game:width 
                                (cosmic-lisp-explorer.game:galaxy game-state)))
                     (< world-y (cosmic-lisp-explorer.game:height 
                                (cosmic-lisp-explorer.game:galaxy game-state))))
            
            ;; Default to empty space
            (charms:write-char-at-point *screen* #\. (+ map-x 1 x) (+ map-y 1 y))
            
            ;; Draw planets
            (dolist (planet (cosmic-lisp-explorer.game:planets current-system))
              (when (and (= (cosmic-lisp-explorer.game:x planet) world-x)
                         (= (cosmic-lisp-explorer.game:y planet) world-y))
                (let ((char (case (cosmic-lisp-explorer.game:object-type planet)
                              (:rocky #\^)
                              (:gas-giant #\O)
                              (:ice #\*)
                              (:earth-like #\@)
                              (:desert #\#)
                              (otherwise #\?)))
                      (color (case (cosmic-lisp-explorer.game:object-type planet)
                               (:rocky 5)
                               (:gas-giant 6)
                               (:ice 4)
                               (:earth-like 3)
                               (:desert 5)
                               (otherwise 1))))
                  (charms:attr-on *screen* (charms:color-pair color))
                  (charms:write-char-at-point *screen* char (+ map-x 1 x) (+ map-y 1 y))
                  (charms:attr-off *screen* (charms:color-pair color)))))
            
            ;; Draw stations
            (dolist (station (cosmic-lisp-explorer.game:stations current-system))
              (when (and (= (cosmic-lisp-explorer.game:x station) world-x)
                         (= (cosmic-lisp-explorer.game:y station) world-y))
                (charms:attr-on *screen* (charms:color-pair 7))
                (charms:write-char-at-point *screen* #\$ (+ map-x 1 x) (+ map-y 1 y))
                (charms:attr-off *screen* (charms:color-pair 7))))
            
            ;; Draw the star at the center of the system
            (when (and (= world-x (cosmic-lisp-explorer.game:x current-system))
                       (= world-y (cosmic-lisp-explorer.game:y current-system)))
              (charms:attr-on *screen* (charms:color-pair 2))
              (charms:write-char-at-point *screen* #\* (+ map-x 1 x) (+ map-y 1 y))
              (charms:attr-off *screen* (charms:color-pair 2)))
            
            ;; Draw player
            (when (and (= world-x (cosmic-lisp-explorer.game:x player))
                       (= world-y (cosmic-lisp-explorer.game:y player)))
              (charms:write-char-at-point *screen* #\@ (+ map-x 1 x) (+ map-y 1 y))))))))
    
    ;; Write instructions
    (charms:write-string-at-point *screen* "Controls: WASD=Move, Space=Interact, I=Inventory, M=Map, Q=Quit" 
                                 2 (+ map-y map-height + 2))))

(defun draw-terminal-player-stats (game-state max-x max-y)
  "Draw player stats on the terminal"
  (let ((player (cosmic-lisp-explorer.game:player game-state))
        (stats-x (- max-x 30))
        (stats-y 4))
    (charms:write-string-at-point *screen* "PLAYER STATUS" stats-x stats-y)
    (charms:write-string-at-point *screen* "-------------" stats-x (1+ stats-y))
    
    (charms:write-string-at-point *screen* 
                                 (format nil "Health: ~A/~A" 
                                         (cosmic-lisp-explorer.game:health player)
                                         (cosmic-lisp-explorer.game:max-health player))
                                 stats-x (+ stats-y 3))
    
    (charms:write-string-at-point *screen* 
                                 (format nil "Fuel:   ~A/~A" 
                                         (cosmic-lisp-explorer.game:fuel player)
                                         (cosmic-lisp-explorer.game:max-fuel player))
                                 stats-x (+ stats-y 4))
    
    (charms:write-string-at-point *screen* 
                                 (format nil "Credits: ~A" 
                                         (cosmic-lisp-explorer.game:credits player))
                                 stats-x (+ stats-y 5))
    
    (charms:write-string-at-point *screen* 
                                 (format nil "Tech:    ~A" 
                                         (cosmic-lisp-explorer.game:tech-level player))
                                 stats-x (+ stats-y 6))))

(defun draw-terminal-log (game-state max-x max-y)
  "Draw the game log on the terminal"
  (let ((log-y (- max-y 12))
        (log-height 10)
        (logs (cosmic-lisp-explorer.game:logs game-state)))
    
    ;; Draw log header
    (charms:write-string-at-point *screen* "LOG" 2 log-y)
    (charms:write-string-at-point *screen* (make-string (- max-x 4) :initial-element #\-) 
                                 2 (1+ log-y))
    
    ;; Draw log messages
    (dotimes (i (min (length logs) log-height))
      (let ((msg-idx (mod (- (length logs) i 1) (length logs))))
        (when (< msg-idx (length logs))
          (charms:write-string-at-point *screen* 
                                       (aref logs msg-idx)
                                       3 (+ log-y 2 i)))))))
