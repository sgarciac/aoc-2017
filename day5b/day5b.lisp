;;;; day5b.lisp

(in-package #:day5b)

(defun read-input (file)
  (let ((entries (with-open-file (in file)
                   (loop for number = (read in nil)
                      while number collect number))))
    (make-array (length entries) :initial-contents entries)))

(defun day5b (input)
  (labels ((update-program (program position)
             (if (>= (aref program position) 3)
                 (decf (aref program position))
                 (incf (aref program position))) 
             program))
    (loop
       for prev-position = 0 then position
       for position = 0 then (+ position (aref program position))
       for program = input then (update-program program prev-position)
       for count = 0 then (1+ count)
       while (and (>= position 0)
                  (< position (length program)))
       finally (return count))))
