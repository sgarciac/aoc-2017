;;;; day15a.lisp
(in-package #:day15a)

(defparameter *cleaner16* #B00000000000000001111111111111111)

(defun match-lower16(x y)
  (= (logand *cleaner16* x)
     (logand *cleaner16* y)))

(defun make-generator(init-val factor)
  (lambda ()
    (setf init-val (rem (* factor init-val) 2147483647))))


