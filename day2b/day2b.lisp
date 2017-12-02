;;;; day2b.lisp
(in-package #:day2b)

(defun read-input (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
       while line collect (with-input-from-string (lin line)
                            (loop for number = (read lin nil) while number collect number)))))


(defun find-divisor (target list)
  (let ((divisor (find-if (lambda (candidate) (or (zerop (mod target candidate))
                                             (zerop (mod candidate target)))) list)))
    divisor))

(defun find-divisors (list)
  (when list
    (let ((divisor (find-divisor (car list) (cdr list))))
      (if divisor
          (/ (max (car list) divisor) (min (car list) divisor))
          (find-divisors (cdr list))))))

(defun day2b (input)
  (loop for line in input
     summing (find-divisors line)))
