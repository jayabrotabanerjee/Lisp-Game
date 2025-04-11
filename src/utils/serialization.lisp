(in-package :cosmic-lisp-explorer.utils)

;; Game state serialization for save/load functionality

(defparameter *save-directory* "saves/")

(defun ensure-save-directory ()
  "Ensure the save directory exists"
  (ensure-directory *save-directory*))

(defun save-game (game-state filename)
  "Save the game state to a file"
  (ensure-save-directory)
  (let ((full-path (merge-pathnames filename *save-directory*)))
    (with-open-file (stream full-path :direction :output 
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((*print-readably* t)
            (*print-circle* t))
        (print (serialize-game-state game-state) stream)))
    t))

(defun load-game (filename)
  "Load a game state from a file"
  (ensure-save-directory)
  (let ((full-path (merge-pathnames filename *save-directory*)))
    (if (probe-file full-path)
        (with-open-file (stream full-path :direction :input)
          (let ((saved-state (read stream)))
            (deserialize-game-state saved-state)))
        nil)))

(defun serialize-entity (entity)
  "Serialize an entity to an S-expression"
  (list :entity
        :class (type-of entity)
        :name (cosmic-lisp-explorer.game:name entity)
        :type (cosmic-lisp-explorer.game:entity-type entity)
        :x (cosmic-lisp-explorer.game:x entity)
        :y (cosmic-lisp-explorer.game:y entity)
        :health (cosmic-lisp-explorer.game:health entity)
        :max-health (cosmic-lisp-explorer.game:max-health entity)
        :inventory (mapcar #'serialize-item (cosmic-lisp-explorer.game:inventory entity))
        :equipment (mapcar #'serialize-item (cosmic-lisp-explorer.game:equipment entity))
        :faction (cosmic-lisp-explorer.game:faction entity)
        :behavior (cosmic-lisp-explorer.game:behavior entity)
        ;; Add additional attributes based on entity type
        :attributes (cond
                      ((typep entity 'cosmic-lisp-explorer.game:player)
                       (list :fuel (cosmic-lisp-explorer.game:fuel entity)
                             :max-fuel (cosmic-lisp-explorer.game:max-fuel entity)
                             :credits (cosmic-lisp-explorer.game:credits entity)
                             :tech-level (cosmic-lisp-explorer.game:tech-level entity)
                             :visited-systems (mapcar #'serialize-system-reference 
                                                    (cosmic-lisp-explorer.game:visited-systems entity))))
                      ((typep entity 'cosmic-lisp-explorer.game:alien-ship)
                       (list :species (cosmic-lisp-explorer.game:species entity)
                             :tech-level (cosmic-lisp-explorer.game:tech-level entity)
                             :hostility (cosmic-lisp-explorer.game:hostility entity)))
                      (t nil))))

(defun serialize-item (item)
  "Serialize an item to an S-expression"
  (list :item
        :class (type-of item)
        :name (cosmic-lisp-explorer.game:name item)
        :type (cosmic-lisp-explorer.game:item-type item)
        :value (cosmic-lisp-explorer.game:value item)
        :effect (cosmic-lisp-explorer.game:effect item)
        :description (cosmic-lisp-explorer.game:description item)
        ;; Add additional attributes based on item type
        :attributes (when (typep item 'cosmic-lisp-explorer.game:ship-component)
                      (list :slot (cosmic-lisp-explorer.game:slot item)
                            :level (cosmic-lisp-explorer.game:level item)
                            :power-consumption (cosmic-lisp-explorer.game:power-consumption item)))))

(defun serialize-celestial-object (object)
  "Serialize a celestial object to an S-expression"
  (list :celestial-object
        :name (cosmic-lisp-explorer.game:name object)
        :type (cosmic-lisp-explorer.game:object-type object)
        :x (cosmic-lisp-explorer.game:x object)
        :y (cosmic-lisp-explorer.game:y object)
        :resources (cosmic-lisp-explorer.game:resources object)
        :inhabitants (cosmic-lisp-explorer.game:inhabitants object)
        :visited (cosmic-lisp-explorer.game:visited object)))

(defun serialize-system (system)
  "Serialize a star system to an S-expression"
  (list :star-system
        :name (cosmic-lisp-explorer.game:name system)
        :x (cosmic-lisp-explorer.game:x system)
        :y (cosmic-lisp-explorer.game:y system)
        :star-type (cosmic-lisp-explorer.game:star-type system)
        :planets (mapcar #'serialize-celestial-object (cosmic-lisp-explorer.game:planets system))
        :stations (mapcar #'serialize-celestial-object (cosmic-lisp-explorer.game:stations system))
        :visited (cosmic-lisp-explorer.game:visited system)))

(defun serialize-system-reference (system)
  "Serialize a reference to a star system (just the identifying information)"
  (list :system-ref
        :name (cosmic-lisp-explorer.game:name system)
        :x (cosmic-lisp-explorer.game:x system)
        :y (cosmic-lisp-explorer.game:y system)))

(defun serialize-galaxy (galaxy)
  "Serialize a galaxy to an S-expression"
  (list :galaxy
        :name (cosmic-lisp-explorer.game:name galaxy)
        :width (cosmic-lisp-explorer.game:width galaxy)
        :height (cosmic-lisp-explorer.game:height galaxy)
        :systems (mapcar #'serialize-system (cosmic-lisp-explorer.game:systems galaxy))))

(defun serialize-game-state (game-state)
  "Serialize the entire game state to an S-expression"
  (list :game-state
        :galaxy (serialize-galaxy (cosmic-lisp-explorer.game:galaxy game-state))
        :player (serialize-entity (cosmic-lisp-explorer.game:player game-state))
        :current-system (serialize-system (cosmic-lisp-explorer.game:current-system game-state))
        :turn-count (cosmic-lisp-explorer.game:turn-count game-state)
        :entities (mapcar #'serialize-entity (cosmic-lisp-explorer.game:entities game-state))
        :logs (coerce (cosmic-lisp-explorer.game:logs game-state) 'list)
        :game-over (cosmic-lisp-explorer.game:game-over game-state)))

;; Deserialization functions would be implemented similarly but are omitted for brevity
;; A complete implementation would include functions like:
;; - deserialize-entity
;; - deserialize-item
;; - deserialize-celestial-object
;; - deserialize-system
;; - deserialize-galaxy
;; - deserialize-game-state
;; These would reconstruct the game objects from the S-expressions

(defun deserialize-game-state (serialized-state)
  "Deserialize a game state from an S-expression (placeholder)"
  ;; This is a simplified placeholder - a full implementation would reconstruct
  ;; the entire game state from the serialized representation
  (format t "Game loading is not fully implemented in this demo.~%")
  (make-instance 'cosmic-lisp-explorer.game:game-state))
