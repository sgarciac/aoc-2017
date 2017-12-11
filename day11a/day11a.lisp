;;;; day11a.lisp
(in-package #:day11a)

(defparameter *deltas*
  '((:N 0 1 -1)
    (:NE 1 0 -1)
    (:SE 1 -1 0)
    (:S 0 -1 1)
    (:SW -1 0 1)
    (:NW -1 1 0)))

(defun get-delta (direction)
  (cdr (assoc direction *deltas*)))

(defun read-input(file)
  (with-open-file (in file)
    (mapcar (lambda (item) (intern (string-upcase item) "KEYWORD")) (cl-ppcre:split "," (read-line in)))))

(defun walk(directions &optional (initial-x 0) (initial-y 0) (initial-z 0))
  (loop
     for x = initial-x then (+ (first delta) x)
     for y = initial-y then (+ (second delta) y)
     for z = initial-z then (+ (third delta) z)
     for direction in directions
     for delta = (get-delta direction)
     finally (return (list x y z))))

(defun distance-from-center(directions)
  (/ (apply #'+ (mapcar #'abs (walk directions))) 2))


				   
       


(read-input "input")







