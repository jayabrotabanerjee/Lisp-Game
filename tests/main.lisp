(in-package :cosmic-lisp-explorer-tests)

(test world-generation-test
  "Test that world generation works correctly"
  (let ((game-state (make-instance 'cosmic-lisp-explorer.game:game-state)))
    (cosmic-lisp-explorer.game:generate-galaxy game-state)
    (let ((galaxy (cosmic-lisp-explorer.game:galaxy game-state)))
      (is (not (null galaxy)) "Galaxy should not be nil")
      (is (> (length (cosmic-lisp-explorer.game:systems galaxy)) 0) 
          "Galaxy should have at least one star system")
      (is (not (null (cosmic-lisp-explorer.game:current-system game-state)))
          "Current system should be set"))))

(test player-initialization-test
  "Test player initialization"
  (let ((game-state (make-instance 'cosmic-lisp-explorer.game:game-state)))
    (cosmic-lisp-explorer.game:generate-galaxy game-state)
    (cosmic-lisp-explorer.game:init-player game-state)
    (let ((player (cosmic-lisp-explorer.game:player game-state)))
      (is (not (null player)) "Player should not be nil")
      (is (= (cosmic-lisp-explorer.game:health player) 100) "Player should have 100 health")
      (is (= (cosmic-lisp-explorer.game:fuel player) 100) "Player should have 100 fuel")
      (is (= (cosmic-lisp-explorer.game:credits player) 500) "Player should have 500 credits"))))

(test player-movement-test
  "Test player movement"
  (let ((game-state (make-instance 'cosmic-lisp-explorer.game:game-state)))
    (cosmic-lisp-explorer.game:generate-galaxy game-state)
    (cosmic-lisp-explorer.game:init-player game-state)
    (let* ((player (cosmic-lisp-explorer.game:player game-state))
           (initial-x (cosmic-lisp-explorer.game:x player))
           (initial-y (cosmic-lisp-explorer.game:y player))
           (initial-fuel (cosmic-lisp-explorer.game:fuel player)))
      
      ;; Move right
      (cosmic-lisp-explorer.game:move-player game-state :right)
      (is (= (cosmic-lisp-explorer.game:x player) (1+ initial-x)) "Player should move right")
      (is (= (cosmic-lisp-explorer.game:y player) initial-y) "Y coordinate should not change")
      (is (= (cosmic-lisp-explorer.game:fuel player) (1- initial-fuel)) "Fuel should decrease by 1")
      
      ;; Move down
      (cosmic-lisp-explorer.game:move-player game-state :down)
      (is (= (cosmic-lisp-explorer.game:x player) (1+ initial-x)) "X coordinate should not change")
      (is (= (cosmic-lisp-explorer.game:y player) (1+ initial-y)) "Player should move down")
      (is (= (cosmic-lisp-explorer.game:fuel player) (- initial-fuel 2)) "Fuel should decrease by 2"))))

(test procedural-generation-test
  "Test procedural generation utilities"
  ;; Test perlin noise
  (let ((noise (cosmic-lisp-explorer.utils:perlin-noise 0.5 0.5)))
    (is (<= -1.0 noise 1.0) "Perlin noise should be between -1 and 1"))
  
  ;; Test random-between
  (dotimes (i 100)
    (let ((value (cosmic-lisp-explorer.utils:random-between 10 20)))
      (is (<= 10 value 20) "random-between should return values in the specified range")))
  
  ;; Test dice rolling
  (let ((roll (cosmic-lisp-explorer.utils:roll-dice 3 6 2)))
    (is (<= 5 roll 20) "3d6+2 should result in values between 5 and 20")))

;; Run all tests
(defun run-tests ()
  (run! 'cosmic-lisp-tests))
