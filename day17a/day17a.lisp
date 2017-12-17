;;;; day17a.lisp

(in-package #:day17a)

(defparameter *max-size* 2017)
(defvar *array*)
(defvar *current-pos*)
(defvar *current-size*)

(defmacro with-array (max-size &body body)
  `(let ((*max-size* ,max-size)
         (*array* (make-array ,max-size :element-type 'fixnum :initial-element -1))
         (*current-pos* 0)
         (*current-size* 1))
     (setf (aref *array* *current-pos*) 0)
     ,@body))

(defun spin (steps)
  (setf *current-pos* (mod (+ *current-pos* steps) *current-size*)))

(defun insert-after-current(val)
  (declare (optimize speed))
  (replace *array* *array* :start1 (+ 2 *current-pos*) :end1 *max-size* :start2 (1+ *current-pos*) :end2 (1- *max-size*))
  (setf (aref *array* (1+ *current-pos*)) val)
  (incf *current-pos*)
  (incf *current-size*))

(defun print-array ()
  (format t "~%")
  (loop for i below *current-size*
     do (format t "~[(~A)~;~A~]," (if (= *current-pos* i) 0 1) (aref *array* i))))

(defun day17a (target spin)
  (with-array (+ 2 target)
    (loop for i from 1 upto target
       do (progn
            (spin spin)
            (insert-after-current i)))
    (format t "~A~%" (aref *array* (1+ *current-pos*)))))

(defun day17b (target spin)
  (loop
     with result = -1
     for size from 0 upto target
     for current-pos = 0 then (1+ (mod (+ current-pos spin) size)) 
     do (progn
          (when (= current-pos 1)
            (setf result size)))
     finally (return result)))
