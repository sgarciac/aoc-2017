;;;; day2a.lisp

(in-package #:day2a)

(defun read-input (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
       while line collect (with-input-from-string (lin line)
                            (loop for number = (read lin nil) while number collect number)))))

(defun day2a (input)
  (loop for line in input
     summing (loop for number in line
                maximizing number into max
                minimizing number into min
                finally (return (- max min)))))



