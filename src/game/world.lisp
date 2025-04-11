(in-package :cosmic-lisp-explorer.game)

(defclass game-state ()
  ((galaxy :accessor galaxy :initform nil)
   (player :accessor player :initform nil)
   (current-system :accessor current-system :initform nil)
   (turn-count :accessor turn-count :initform 0)
   (entities :accessor entities :initform nil)
   (logs :accessor logs :initform (make-array 10 :initial-element "" :fill-pointer 0))
   (game-over :accessor game-over :initform nil)))

(defclass celestial-object ()
  ((name :accessor name :initarg :name)
   (type :accessor object-type :initarg :type)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (resources :accessor resources :initarg :resources :initform nil)
   (inhabitants :accessor inhabitants :initarg :inhabitants :initform nil)
   (visited :accessor visited :initarg :visited :initform nil)))

(defclass star-system ()
  ((name :accessor name :initarg :name)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (star-type :accessor star-type :initarg :star-type)
   (planets :accessor planets :initarg :planets :initform nil)
   (stations :accessor stations :initarg :stations :initform nil)
   (visited :accessor visited :initarg :visited :initform nil)))

(defclass galaxy ()
  ((name :accessor name :initarg :name)
   (systems :accessor systems :initarg :systems :initform nil)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defun generate-star-name ()
  "Generate a random star name"
  (let ((prefixes '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta"))
        (suffixes '("Centauri" "Cygni" "Draconis" "Eridani" "Lyrae" "Orionis" "Persei")))
    (format nil "~A ~A-~D" 
            (nth (random (length prefixes)) prefixes)
            (nth (random (length suffixes)) suffixes)
            (1+ (random 9)))))

(defun generate-planet (system-x system-y index)
  "Generate a random planet for a star system"
  (let* ((planet-types '(:rocky :gas-giant :ice :earth-like :desert))
         (type (nth (random (length planet-types)) planet-types))
         (resources (case type
                      (:rocky '(:minerals :metals))
                      (:gas-giant '(:gas :energy))
                      (:ice '(:water :minerals))
                      (:earth-like '(:food :water :life))
                      (:desert '(:minerals :energy))))
         (orbit-distance (+ 2 (* index 3)))
         (angle (random (* 2 pi)))
         (x (+ system-x (round (* orbit-distance (cos angle)))))
         (y (+ system-y (round (* orbit-distance (sin angle))))))
    (make-instance 'celestial-object
                   :name (format nil "Planet ~A" (code-char (+ (char-code #\A) index)))
                   :type type
                   :x x
                   :y y
                   :resources resources)))

(defun generate-star-system (galaxy x y)
  "Generate a random star system"
  (let* ((star-types '(:red-dwarf :yellow-dwarf :blue-giant :white-dwarf :neutron))
         (star-type (nth (random (length star-types)) star-types))
         (planet-count (+ 1 (random 5)))
         (system (make-instance 'star-system
                               :name (generate-star-name)
                               :x x
                               :y y
                               :star-type star-type)))
    ;; Add planets
    (setf (planets system)
          (loop for i from 0 below planet-count
                collect (generate-planet x y i)))
    
    ;; Maybe add a space station (30% chance)
    (when (< (random 100) 30)
      (let ((station-angle (random (* 2 pi)))
            (station-distance (+ 1 (random 3))))
        (setf (stations system)
              (list (make-instance 'celestial-object
                                  :name "Space Station"
                                  :type :station
                                  :x (+ x (round (* station-distance (cos station-angle))))
                                  :y (+ y (round (* station-distance (sin station-angle)))))))))
    system))

(defun generate-galaxy (game-state)
  "Generate a new galaxy"
  (let* ((width 100)
         (height 100)
         (system-count 25)
         (new-galaxy (make-instance 'galaxy
                                   :name "Andromeda Sector"
                                   :width width
                                   :height height)))
    
    ;; Create star systems with minimal distance between them
    (let ((systems nil)
          (min-distance 10))
      (dotimes (i system-count)
        (let ((valid-position nil)
              (x 0)
              (y 0))
          (loop until valid-position do
            (setf x (+ 5 (random (- width 10)))
                  y (+ 5 (random (- height 10))))
            
            (setf valid-position t)
            (dolist (existing-system systems)
              (let ((dist (sqrt (+ (expt (- x (x existing-system)) 2)
                                  (expt (- y (y existing-system)) 2)))))
                (when (< dist min-distance)
                  (setf valid-position nil)
                  (return)))))
          
          (push (generate-star-system new-galaxy x y) systems)))
      
      (setf (systems new-galaxy) (nreverse systems)))
    
    (setf (galaxy game-state) new-galaxy)
    (setf (current-system game-state) (car (systems new-galaxy)))))

(defun add-log-message (game-state message)
  "Add a message to the game log"
  (let ((logs (logs game-state)))
    (when (= (length logs) (array-dimension logs 0))
      (vector-pop logs))
    (vector-push-extend message logs)))
