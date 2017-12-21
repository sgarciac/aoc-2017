;;;; day15a.lisp
(in-package #:day15a)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defparameter *cleaner16* #B00000000000000001111111111111111)

(defun match-lower16(x y)
  (declare (type (unsigned-byte 32) x y *cleaner16*))
  (= (logand *cleaner16* x)
     (logand *cleaner16* y)))

(defun make-generator(init-val factor)
  (declare (type (unsigned-byte 32) init-val factor *cleaner16*))
  (lambda ()
    (setf init-val (rem (* factor init-val) 2147483647))))



