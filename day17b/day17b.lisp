;;;; day17b.lisp
(in-package #:day17b)

(defun day17b (target spin)
  (loop
     with result = -1
     for size from 0 upto target
     for current-pos = 0 then (1+ (mod (+ current-pos spin) size)) 
     do (progn
          (when (= current-pos 1)
            (setf result size)))
     finally (return result)))
