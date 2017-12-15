;;;; day15b.lisp

(in-package #:day15b)

(defparameter *cleaner16* #B00000000000000001111111111111111)

(defun match-lower16(x y)
  (= (logand *cleaner16* x)
     (logand *cleaner16* y)))

(defun make-generator-2(init-val factor divisor)
  (lambda ()
    (setf init-val
	  (loop
	     for candidate = (rem (* factor init-val) 2147483647) then (rem (* factor candidate) 2147483647)
	     while (not (zerop (mod candidate divisor)))
	     finally (return candidate)))))
