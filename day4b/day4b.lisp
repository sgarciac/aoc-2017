;;;; day4a.lisp

(in-package #:day4b)

;;; "day4a" goes here. Hacks and glory await!
(defun read-input (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
       while line collect (split-sequence #\Space line))))



(defun has-duplicates-p (entries)
  (labels ((helper (list)
	     (when (and list (cdr list))
	       (or (string= (car list) (cadr list))
		   (has-duplicates-p (cdr list))))))
    (let ((sorted-entries (sort (mapcar (lambda (word) (sort word #'char-lessp)) entries) #'string-lessp)))
      (helper sorted-entries))))

(defun day4b (input)
  (- (length input)(count-if #'has-duplicates-p input)))





