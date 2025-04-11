(in-package :cosmic-lisp-explorer.utils)

;; Perlin noise implementation for procedural generation
;; Simplified version based on Ken Perlin's improved algorithm

(defparameter *permutation*
  (let ((p (make-array 512)))
    (dotimes (i 256)
      (setf (aref p i) i))
    
    ;; Shuffle
    (dotimes (i 255)
      (let* ((j (+ i (random (- 256 i))))
             (temp (aref p i)))
        (setf (aref p i) (aref p j))
        (setf (aref p j) temp)))
    
    ;; Duplicate
    (dotimes (i 256)
      (setf (aref p (+ i 256)) (aref p i)))
    
    p))

(defun fade (t)
  "Fade function for perlin noise"
  (* t t t (+ (* t (- (* t 6) 15)) 10)))

(defun lerp (t a b)
  "Linear interpolation"
  (+ a (* t (- b a))))

(defun grad (hash x y z)
  "Gradient function for perlin noise"
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (zerop (logand h 1)) u (- u))
       (if (zerop (logand h 2)) v (- v)))))

(defun perlin-noise (x y &optional (z 0.0))
  "Generate a perlin noise value at coordinates (x, y, z)"
  (let* ((xi (logand (floor x) 255))
         (yi (logand (floor y) 255))
         (zi (logand (floor z) 255))
         (xf (- x (floor x)))
         (yf (- y (floor y)))
         (zf (- z (floor z)))
         (u (fade xf))
         (v (fade yf))
         (w (fade zf))
         (aaa (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi) yi)) zi)))
         (aba (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi) yi+1)) zi)))
         (aab (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi) yi)) zi+1)))
         (abb (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi) yi+1)) zi+1)))
         (baa (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi+1) yi)) zi)))
         (bba (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi+1) yi+1)) zi)))
         (bab (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi+1) yi)) zi+1)))
         (bbb (aref *permutation* (+ (aref *permutation* (+ (aref *permutation* xi+1) yi+1)) zi+1))))
    (lerp w
          (lerp v
                (lerp u
                      (grad aaa xf yf zf)
                      (grad baa (1- xf) yf zf))
                (lerp u
                      (grad aba xf (1- yf) zf)
                      (grad bba (1- xf) (1- yf) zf)))
          (lerp v
                (lerp u
                      (grad aab xf yf (1- zf))
                      (grad bab (1- xf) yf (1- zf)))
                (lerp u
                      (grad abb xf (1- yf) (1- zf))
                      (grad bbb (1- xf) (1- yf) (1- zf)))))))

;; Cellular automata for generating interesting patterns
(defun cellular-automata (width height iterations &key (birth-rule '(3)) (survival-rule '(2 3)))
  "Generate a pattern using cellular automata (Conway's Game of Life rules by default)"
  (let ((grid (make-array (list width height) :initial-element nil))
        (temp-grid (make-array (list width height))))
    
    ;; Initialize with random cells (around 25% alive)
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref grid x y) (< (random 100) 25))))
    
    ;; Run iterations
    (dotimes (_ iterations)
      (dotimes (y height)
        (dotimes (x width)
          (let ((neighbors (count-neighbors grid x y width height)))
            (setf (aref temp-grid x y)
                  (if (aref grid x y)
                      (member neighbors survival-rule)
                      (member neighbors birth-rule))))))
      
      ;; Copy temp grid back to main grid
      (dotimes (y height)
        (dotimes (x width)
          (setf (aref grid x y) (aref temp-grid x y)))))
    
    grid))

(defun count-neighbors (grid x y width height)
  "Count live neighbors in a cellular automata grid"
  (let ((count 0))
    (dolist (dy '(-1 0 1))
      (dolist (dx '(-1 0 1))
        (unless (and (zerop dx) (zerop dy))
          (let ((nx (mod (+ x dx) width))
                (ny (mod (+ y dy) height)))
            (when (aref grid nx ny)
              (incf count))))))
    count))

;; Utility functions for random generation
(defun random-between (min max)
  "Generate a random number between min and max (inclusive)"
  (+ min (random (1+ (- max min)))))

(defun roll-dice (num-dice sides &optional (modifier 0))
  "Roll dice (e.g., 3d6+2)"
  (+ (loop repeat num-dice sum (1+ (random sides)))
     modifier))
