(in-package :cosmic-lisp-explorer.utils)

(defun string-to-keyword (string)
  "Convert a string to a keyword"
  (intern (string-upcase string) :keyword))

(defun keyword-to-string (keyword)
  "Convert a keyword to a string"
  (string-downcase (symbol-name keyword)))

(defun distance (x1 y1 x2 y2)
  "Calculate Euclidean distance between points"
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(defun manhattan-distance (x1 y1 x2 y2)
  "Calculate Manhattan distance between points"
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defun angle-between (x1 y1 x2 y2)
  "Calculate angle between two points in radians"
  (atan (- y2 y1) (- x2 x1)))

(defun clamp (value min-val max-val)
  "Clamp a value between min and max"
  (max min-val (min value max-val)))

(defun format-time (seconds)
  "Format seconds into a readable time string (HH:MM:SS)"
  (multiple-value-bind (hours remainder)
      (floor seconds 3600)
    (multiple-value-bind (minutes seconds)
        (floor remainder 60)
      (format nil "~2,'0D:~2,'0D:~2,'0D" hours minutes seconds))))

(defun ensure-directory (path)
  "Ensure a directory exists, creating it if necessary"
  (ensure-directories-exist (concatenate 'string path "/")))

(defun file-exists-p (filename)
  "Check if a file exists"
  (probe-file filename))

(defun read-file-as-string (filename)
  "Read a file into a string"
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-string-to-file (string filename)
  "Write a string to a file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-sequence string stream)))

(defun split-string (string delimiter)
  "Split a string by delimiter"
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun join-strings (strings delimiter)
  "Join strings with a delimiter"
  (format nil (concatenate 'string "~{~A~^" delimiter "~}") strings))

(defun generate-unique-id ()
  "Generate a unique ID (using a simple method)"
  (format nil "~X" (random (expt 16 8))))

(defun current-timestamp ()
  "Get the current timestamp as an integer"
  (get-universal-time))

(defun shuffle-list (list)
  "Return a randomly shuffled copy of the list"
  (let ((result (copy-list list)))
    (loop for i from (length result) downto 2
          do (rotatef (nth (random i) result)
                      (nth (1- i) result)))
    result))

(defun choose-random (list)
  "Choose a random element from a list"
  (nth (random (length list)) list))

(defun weighted-choice (choices weights)
  "Make a weighted random choice from a list.
   Choices and weights should be parallel lists."
  (assert (= (length choices) (length weights)))
  (let* ((total-weight (reduce #'+ weights))
         (random-value (random total-weight))
         (cumulative-weight 0))
    (loop for choice in choices
          for weight in weights
          do (incf cumulative-weight weight)
          when (< random-value cumulative-weight)
          return choice)))
