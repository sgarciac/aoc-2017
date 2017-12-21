;;;; day20a.lisp

(in-package #:day20a)

(defstruct value
  x
  y
  z)

(defstruct particle
  id
  pos
  speed
  acc)

(defun parse-particle(line id)
  (let ((numbers (mapcar #'parse-integer (remove "" (cl-ppcre:split "[^\\d-]" line) :test #'string=))))
    (make-particle
     :id id
     :pos (make-value :x (nth 0 numbers) :y (nth 1 numbers) :z (nth 2 numbers))
     :speed (make-value :x (nth 3 numbers) :y (nth 4 numbers) :z (nth 5 numbers))
     :acc (make-value :x (nth 6 numbers) :y (nth 7 numbers) :z (nth 8 numbers)))))

(defun sum-abs(value)
  (+ (abs (value-x value)) (abs (value-y value)) (abs (value-z value))))

(defun read-input(file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       for i = 0 then (1+ i)
       while line collect (parse-particle line i))))

(defun day20a (input)
  (first (sort input (lambda (p1 p2) (< (abs (sum-abs (particle-acc p1))) (abs (sum-abs (particle-acc p2))))))))
		  

