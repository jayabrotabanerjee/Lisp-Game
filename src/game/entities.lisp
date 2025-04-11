(in-package :cosmic-lisp-explorer.game)

(defclass entity ()
  ((name :accessor name :initarg :name)
   (type :accessor entity-type :initarg :type)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (health :accessor health :initarg :health :initform 100)
   (max-health :accessor max-health :initarg :max-health :initform 100)
   (inventory :accessor inventory :initarg :inventory :initform nil)
   (equipment :accessor equipment :initarg :equipment :initform nil)
   (faction :accessor faction :initarg :faction :initform nil)
   (behavior :accessor behavior :initarg :behavior :initform :neutral)))

(defclass player (entity)
  ((fuel :accessor fuel :initarg :fuel :initform 100)
   (max-fuel :accessor max-fuel :initarg :max-fuel :initform 100)
   (credits :accessor credits :initarg :credits :initform 500)
   (visited-systems :accessor visited-systems :initarg :visited-systems :initform nil)
   (current-quest :accessor current-quest :initarg :current-quest :initform nil)
   (tech-level :accessor tech-level :initarg :tech-level :initform 1)))

(defclass alien-ship (entity)
  ((species :accessor species :initarg :species)
   (tech-level :accessor tech-level :initarg :tech-level)
   (hostility :accessor hostility :initarg :hostility)))

(defclass item ()
  ((name :accessor name :initarg :name)
   (type :accessor item-type :initarg :type)
   (value :accessor value :initarg :value)
   (effect :accessor effect :initarg :effect :initform nil)
   (description :accessor description :initarg :description)))

(defclass ship-component (item)
  ((slot :accessor slot :initarg :slot)
   (level :accessor level :initarg :level)
   (power-consumption :accessor power-consumption :initarg :power-consumption)))

(defun make-entity (type name x y &rest args)
  "Factory function to create entities"
  (apply #'make-instance 
         (ecase type
           (:player 'player)
           (:alien 'alien-ship)
           (:item 'item)
           (:component 'ship-component))
         :name name :type type :x x :y y args))

(defun init-player (game-state)
  "Initialize the player entity"
  (let* ((galaxy (galaxy game-state))
         (starting-system (car (systems galaxy)))
         (player (make-entity :player "Explorer" (x starting-system) (y starting-system))))
    
    ;; Add basic equipment
    (setf (equipment player)
          (list (make-instance 'ship-component
                              :name "Basic Engine"
                              :type :component
                              :slot :engine
                              :level 1
                              :power-consumption 10
                              :value 200)
                (make-instance 'ship-component
                              :name "Light Laser"
                              :type :component
                              :slot :weapon
                              :level 1
                              :power-consumption 15
                              :value 250)))
    
    ;; Add starting inventory
    (setf (inventory player)
          (list (make-instance 'item
                              :name "Repair Kit"
                              :type :consumable
                              :value 50
                              :effect '(:health 25)
                              :description "Repairs 25 hull damage")
                (make-instance 'item
                              :name "Fuel Cell"
                              :type :consumable
                              :value 30
                              :effect '(:fuel 20)
                              :description "Restores 20 fuel")))
    
    (setf (player game-state) player)
    (setf (visited-systems player) (list starting-system))
    (setf (visited starting-system) t)))

(defun entity-at (game-state x y)
  "Find an entity at the given coordinates"
  (let ((all-entities (entities game-state)))
    (find-if (lambda (entity)
               (and (= (x entity) x)
                    (= (y entity) y)))
             all-entities)))

(defun move-entity (entity dx dy)
  "Move an entity by the given delta"
  (incf (x entity) dx)
  (incf (y entity) dy))

(defun move-player (game-state direction)
  "Move the player in the given direction"
  (let ((player (player game-state))
        (dx 0)
        (dy 0))
    (ecase direction
      (:up (setf dy -1))
      (:down (setf dy 1))
      (:left (setf dx -1))
      (:right (setf dx 1)))
    
    (let ((new-x (+ (x player) dx))
          (new-y (+ (y player) dy)))
      ;; Check if movement is valid
      (if (can-move-to-p game-state new-x new-y)
          (progn
            (move-entity player dx dy)
            (decf (fuel player) 1)
            (when (< (fuel player) 0)
              (setf (fuel player) 0)
              (add-log-message game-state "WARNING: Out of fuel!"))
            t)
          (progn
            (add-log-message game-state "Cannot move there!")
            nil)))))

(defun can-move-to-p (game-state x y)
  "Check if the player can move to the given coordinates"
  (let ((current-system (current-system game-state)))
    ;; Simple boundary check for demo
    (and (>= x 0)
         (>= y 0)
         (< x (width (galaxy game-state)))
         (< y (height (galaxy game-state))))))

(defun process-turn (game-state input)
  "Process a game turn based on player input"
  (incf (turn-count game-state))
  (let ((player (player game-state)))
    (case input
      ((:up :down :left :right)
       (move-player game-state input))
      (:interact
       (interact-with-object game-state))
      (:inventory
       (display-inventory game-state))
      (:map
       (display-map game-state))
      (t nil))))

(defun interact-with-object (game-state)
  "Interact with an object at player's position"
  (let* ((player (player game-state))
         (objects-here (find-objects-at game-state (x player) (y player))))
    (when objects-here
      (let ((object (car objects-here)))
        (case (object-type object)
          (:station (visit-station game-state object))
          (:planet (explore-planet game-state object))
          (otherwise
           (add-log-message game-state (format nil "Nothing to interact with."))))))))

(defun find-objects-at (game-state x y)
  "Find all objects at the given coordinates"
  (let ((current-system (current-system game-state))
        (results nil))
    ;; Check planets
    (dolist (planet (planets current-system))
      (when (and (= (x planet) x)
                 (= (y planet) y))
        (push planet results)))
    
    ;; Check stations
    (dolist (station (stations current-system))
      (when (and (= (x station) x)
                 (= (y station) y))
        (push station results)))
    
    results))

(defun visit-station (game-state station)
  (setf (visited station) t)
  (add-log-message game-state (format nil "Docked at ~A." (name station))))

(defun explore-planet (game-state planet)
  (setf (visited planet) t)
  (add-log-message game-state (format nil "Landed on ~A." (name planet))))

(defun display-inventory (game-state)
  ;; This is just a placeholder - actual display happens in UI layer
  (add-log-message game-state "Opening inventory..."))

(defun display-map (game-state)
  ;; This is just a placeholder - actual display happens in UI layer
  (add-log-message game-state "Opening star map..."))
