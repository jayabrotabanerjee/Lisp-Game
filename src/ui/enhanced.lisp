(in-package :cosmic-lisp-explorer.ui)

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *game-initialized* nil)

(defun init-enhanced-ui ()
  "Initialize the enhanced UI using cl-raylib"
  (raylib:init-window *window-width* *window-height* "Cosmic Lisp Explorer")
  (raylib:set-target-fps 60)
  (load-textures)
  (setf *game-initialized* t))

(defun shutdown-enhanced-ui ()
  "Shut down enhanced UI"
  (unload-textures)
  (raylib:close-window))

(defparameter *textures* (make-hash-table :test 'eq))

(defun load-textures ()
  "Load game textures"
  (setf (gethash :player *textures*) (raylib:load-texture "assets/sprites/ship.png"))
  (setf (gethash :planet-rocky *textures*) (raylib:load-texture "assets/sprites/planet_rocky.png"))
  (setf (gethash :planet-gas *textures*) (raylib:load-texture "assets/sprites/planet_gas.png"))
  (setf (gethash :planet-ice *textures*) (raylib:load-texture "assets/sprites/planet_ice.png"))
  (setf (gethash :planet-earth *textures*) (raylib:load-texture "assets/sprites/planet_earth.png"))
  (setf (gethash :planet-desert *textures*) (raylib:load-texture "assets/sprites/planet_desert.png"))
  (setf (gethash :star-red *textures*) (raylib:load-texture "assets/sprites/star_red.png"))
  (setf (gethash :star-yellow *textures*) (raylib:load-texture "assets/sprites/star_yellow.png"))
  (setf (gethash :star-blue *textures*) (raylib:load-texture "assets/sprites/star_blue.png"))
  (setf (gethash :star-white *textures*) (raylib:load-texture "assets/sprites/star_white.png"))
  (setf (gethash :star-neutron *textures*) (raylib:load-texture "assets/sprites/star_neutron.png"))
  (setf (gethash :station *textures*) (raylib:load-texture "assets/sprites/station.png"))
  (setf (gethash :background *textures*) (raylib:load-texture "assets/sprites/background.png")))

(defun unload-textures ()
  "Unload game textures"
  (maphash (lambda (key texture)
             (declare (ignore key))
             (raylib:unload-texture texture))
           *textures*))

(defun handle-enhanced-input ()
  "Handle keyboard input in enhanced graphics mode"
  (cond
    ((raylib:is-key-pressed raylib:+key-q+) :quit)
    ((raylib:is-key-pressed raylib:+key-w+) :up)
    ((raylib:is-key-pressed raylib:+key-s+) :down)
    ((raylib:is-key-pressed raylib:+key-a+) :left)
    ((raylib:is-key-pressed raylib:+key-d+) :right)
    ((raylib:is-key-pressed raylib:+key-space+) :interact)
    ((raylib:is-key-pressed raylib:+key-i+) :inventory)
    ((raylib:is-key-pressed raylib:+key-m+) :map)
    ((raylib:is-key-pressed raylib:+key-e+) :equipment)
    ((raylib:is-key-pressed raylib:+key-v+) :save)
    (t nil)))

(defun render-enhanced-game (game-state)
  "Render the game with enhanced graphics"
  (raylib:begin-drawing)
  (raylib:clear-background raylib:+black+)
  
  ;; Draw background
  (let ((bg-texture (gethash :background *textures*)))
    (raylib:draw-texture bg-texture 0 0 raylib:+white+))
  
  ;; Draw the current system
  (let ((current-system (cosmic-lisp-explorer.game:current-system game-state)))
    (when current-system
      ;; Draw the system name
      (raylib:draw-text (format nil "Star System: ~A" 
                               (cosmic-lisp-explorer.game:name current-system))
                       20 20 20 raylib:+white+)
      
      ;; Draw the map
      (draw-enhanced-map game-state)
      
      ;; Draw UI elements
      (draw-enhanced-ui game-state)))
  
  (raylib:end-drawing))

(defun draw-enhanced-map (game-state)
  "Draw the system map with enhanced graphics"
  (let* ((player (cosmic-lisp-explorer.game:player game-state))
         (current-system (cosmic-lisp-explorer.game:current-system game-state))
         (map-width 600)
         (map-height 400)
         (map-x 100)
         (map-y 50)
         (center-x (cosmic-lisp-explorer.game:x player))
         (center-y (cosmic-lisp-explorer.game:y player))
         (viewport-width 50)
         (viewport-height 30)
         (start-x (- center-x (floor viewport-width 2)))
         (start-y (- center-y (floor viewport-height 2)))
         (cell-size 12))
    
    ;; Draw map background
    (raylib:draw-rectangle map-x map-y map-width map-height (raylib:fade raylib:+black+ 0.8))
    (raylib:draw-rectangle-lines map-x map-y map-width map-height raylib:+darkgray+)
    
    ;; Draw grid
    (dotimes (y viewport-height)
      (dotimes (x viewport-width)
        (let ((world-x (+ start-x x))
              (world-y (+ start-y y)))
          (when (and (>= world-x 0) (>= world-y 0)
                     (< world-x (cosmic-lisp-explorer.game:width 
                                (cosmic-lisp-explorer.game:galaxy game-state)))
                     (< world-y (cosmic-lisp-explorer.game:height 
                                (cosmic-lisp-explorer.game:galaxy game-state))))
            
            (let ((screen-x (+ map-x (* x cell-size)))
                  (screen-y (+ map-y (* y cell-size))))
              
              ;; Draw empty space (grid)
              (raylib:draw-rectangle-lines screen-x screen-y cell-size cell-size 
                                         (raylib:fade raylib:+darkgray+ 0.3))
              
              ;; Draw the star at the center of the system
              (when (and (= world-x (cosmic-lisp-explorer.game:x current-system))
                         (= world-y (cosmic-lisp-explorer.game:y current-system)))
                (let* ((star-type (cosmic-lisp-explorer.game:star-type current-system))
                       (texture-key (case star-type
                                      (:red-dwarf :star-red)
                                      (:yellow-dwarf :star-yellow)
                                      (:blue-giant :star-blue)
                                      (:white-dwarf :star-white)
                                      (:neutron :star-neutron)
                                      (otherwise :star-yellow)))
                       (texture (gethash texture-key *textures*)))
                  (raylib:draw-texture-ex texture 
                                        (raylib:make-vector2 :x (+ screen-x (/ cell-size 2))
                                                           :y (+ screen-y (/ cell-size 2)))
                                        0.0 ; rotation
                                        0.3 ; scale
                                        raylib:+white+)))
              
              ;; Draw planets
              (dolist (planet (cosmic-lisp-explorer.game:planets current-system))
                (when (and (= (cosmic-lisp-explorer.game:x planet) world-x)
                           (= (cosmic-lisp-explorer.game:y planet) world-y))
                  (let* ((planet-type (cosmic-lisp-explorer.game:object-type planet))
                         (texture-key (case planet-type
                                        (:rocky :planet-rocky)
                                        (:gas-giant :planet-gas)
                                        (:ice :planet-ice)
                                        (:earth-like :planet-earth)
                                        (:desert :planet-desert)
                                        (otherwise :planet-rocky)))
                         (texture (gethash texture-key *textures*)))
                    (raylib:draw-texture-ex texture 
                                          (raylib:make-vector2 :x (+ screen-x (/ cell-size 2))
                                                             :y (+ screen-y (/ cell-size 2)))
                                          0.0 ; rotation
                                          0.25 ; scale
                                          raylib:+white+))))
              
              ;; Draw stations
              (dolist (station (cosmic-lisp-explorer.game:stations current-system))
                (when (and (= (cosmic-lisp-explorer.game:x station) world-x)
                           (= (cosmic-lisp-explorer.game:y station) world-y))
                  (let ((texture (gethash :station *textures*)))
                    (raylib:draw-texture-ex texture 
                                          (raylib:make-vector2 :x (+ screen-x (/ cell-size 2))
                                                             :y (+ screen-y (/ cell-size 2)))
                                          0.0 ; rotation
                                          0.25 ; scale
                                          raylib:+white+))))
              
              ;; Draw player
              (when (and (= world-x (cosmic-lisp-explorer.game:x player))
                         (= world-y (cosmic-lisp-explorer.game:y player)))
                (let ((texture (gethash :player *textures*)))
                  (raylib:draw-texture-ex texture 
                                        (raylib:make-vector2 :x (+ screen-x (/ cell-size 2))
                                                           :y (+ screen-y (/ cell-size 2)))
                                        0.0 ; rotation
                                        0.25 ; scale
                                        raylib:+white+)))))))))
    
    ;; Draw instructions
    (raylib:draw-text "Controls: WASD=Move, Space=Interact, I=Inventory, M=Map, Q=Quit" 
                     map-x (+ map-y map-height + 10) 20 raylib:+white+)))

(defun draw-enhanced-ui (game-state)
  "Draw UI elements with enhanced graphics"
  (let ((player (cosmic-lisp-explorer.game:player game-state))
        (panel-x 720)
        (panel-y 50)
        (panel-width 200)
        (panel-height 500))
    
    ;; Draw stats panel
    (raylib:draw-rectangle panel-x panel-y panel-width panel-height 
                         (raylib:fade raylib:+darkblue+ 0.8))
    (raylib:draw-rectangle-lines panel-x panel-y panel-width panel-height raylib:+blue+)
    
    ;; Draw player stats
    (raylib:draw-text "PLAYER STATUS" (+ panel-x 10) (+ panel-y 10) 20 raylib:+white+)
    (raylib:draw-line (+ panel-x 10) (+ panel-y 40) (+ panel-x panel-width -10) (+ panel-y 40) raylib:+lightgray+)
    
    ;; Health bar
    (raylib:draw-text "Health:" (+ panel-x 10) (+ panel-y 60) 16 raylib:+white+)
    (let* ((health-pct (/ (cosmic-lisp-explorer.game:health player) 
                         (cosmic-lisp-explorer.game:max-health player)))
           (bar-width 160)
           (bar-height 20)
           (bar-x (+ panel-x 20))
           (bar-y (+ panel-y 85)))
      (raylib:draw-rectangle bar-x bar-y bar-width bar-height raylib:+darkgray+)
      (raylib:draw-rectangle bar-x bar-y (* bar-width health-pct) bar-height raylib:+red+)
      (raylib:draw-text (format nil "~A/~A" 
                               (cosmic-lisp-explorer.game:health player)
                               (cosmic-lisp-explorer.game:max-health player))
                       (+ bar-x 60) (+ bar-y 2) 16 raylib:+white+))
    
    ;; Fuel bar
    (raylib:draw-text "Fuel:" (+ panel-x 10) (+ panel-y 120) 16 raylib:+white+)
    (let* ((fuel-pct (/ (cosmic-lisp-explorer.game:fuel player) 
                       (cosmic-lisp-explorer.game:max-fuel player)))
           (bar-width 160)
           (bar-height 20)
           (bar-x (+ panel-x 20))
           (bar-y (+ panel-y 145)))
      (raylib:draw-rectangle bar-x bar-y bar-width bar-height raylib:+darkgray+)
      (raylib:draw-rectangle bar-x bar-y (* bar-width fuel-pct) bar-height raylib:+blue+)
      (raylib:draw-text (format nil "~A/~A" 
                               (cosmic-lisp-explorer.game:fuel player)
                               (cosmic-lisp-explorer.game:max-fuel player))
                       (+ bar-x 60) (+ bar-y 2) 16 raylib:+white+))
    
    ;; Credits
    (raylib:draw-text (format nil "Credits: ~A" 
                             (cosmic-lisp-explorer.game:credits player))
                     (+ panel-x 10) (+ panel-y 180) 16 raylib:+white+)
    
    ;; Tech level
    (raylib:draw-text (format nil "Tech Level: ~A" 
                             (cosmic-lisp-explorer.game:tech-level player))
                     (+ panel-x 10) (+ panel-y 210) 16 raylib:+white+)
    
    ;; Draw game log
    (raylib:draw-text "LOG" (+ panel-x 10) (+ panel-y 250) 20 raylib:+white+)
    (raylib:draw-line (+ panel-x 10) (+ panel-y 280) (+ panel-x panel-width -10) (+ panel-y 280) raylib:+lightgray+)
    
    (let ((logs (cosmic-lisp-explorer.game:logs game-state))
          (log-y (+ panel-y 290))
          (max-logs 8))
      (dotimes (i (min (length logs) max-logs))
        (let ((msg-idx (mod (- (length logs) i 1) (length logs))))
          (when (< msg-idx (length logs))
            (raylib:draw-text (aref logs msg-idx)
                             (+ panel-x 10) (+ log-y (* i 20)) 12 raylib:+white+)))))))
