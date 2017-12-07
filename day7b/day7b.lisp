;;;; day7b.lisp
(in-package #:day7b)

(defstruct input-entry
  name
  weight
  children)

(defstruct aoc-tree
  name
  weight
  children)

(defun parse-input-entry (line)
  (let ((items (remove "" (cl-ppcre:split "[^\\w]" line :omit-unmatched-p t) :test #'string=)))
    (make-input-entry
     :name (first items)
     :weight (parse-integer (second items))
     :children (cddr items)
     )))

(defun read-input(file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line collect (parse-input-entry line))))

(defun parents (entries)
  (loop
     with parents = (make-hash-table :test #'equal)
     for entry in entries do (progn
			       (unless (gethash (input-entry-name entry) parents)
				 (setf (gethash (input-entry-name entry) parents) nil))
			       (loop
				for child in (input-entry-children entry)
				       do (setf (gethash child parents) (input-entry-name entry))))
       
     finally (return parents)))

(defun find-entry (name entries)
  (find-if (lambda (entry) (string= (input-entry-name entry) name)) entries))

  

(defun find-base-name (parents)
  (loop for key being the hash-keys of parents
     do (when (not (gethash key parents)) (return key))))

(defun tower-weight(aoct)
  (apply #'+ (aoc-tree-weight aoct)
	 (mapcar #'tower-weight (aoc-tree-children aoct))))

(defun aoc-tree-from-entries (name entries)
  (let* ((entry (find-entry name entries)))
    (make-aoc-tree :name name
		   :weight (input-entry-weight entry)
		   :children (mapcar (lambda (child-name)
				       (aoc-tree-from-entries child-name entries))
				     (input-entry-children entry)))))

(defun find-different-weight (weights)
  (let ((len (length weights)))
    (cond ((= 1 len)
	   nil)
	  ((= 2 len)
	   (if (= (car weights) (cadr weights))
	       nil
	       (error "two different weights!")))
	  (t (find-if (lambda (weight)
			(= 1 (count weight weights))) weights)))))

(defun find-first-correction (aoct)
  (let ((len (length (aoc-tree-children aoct))))
    (if (or (zerop len) (= 1 len))
	nil
	(let* ((tower-weights (mapcar #'tower-weight (aoc-tree-children aoct)))
	       (different-weight (find-different-weight tower-weights)))
	  (if different-weight
	      (let ((sub-correction (loop
				       for subtree in (aoc-tree-children aoct)
				       for correction = (find-first-correction subtree)
				       while (not correction)
				       finally (return correction))))
		(if sub-correction
		    sub-correction
		    (let* ((different-tower
			    (find-if (lambda (tower) (= (tower-weight tower) different-weight)) (aoc-tree-children aoct)))
		     (goal-tower-weight (find-if (lambda (weight) (not (= weight different-weight))) tower-weights))
		     (children-weights-sum (apply #'+ (mapcar #'tower-weight (aoc-tree-children different-tower)))))
		      (print tower-weights)
		      (format t "(parent ~A) (~A) ~A: -> ~A ~%" (aoc-tree-name aoct) (aoc-tree-weight different-tower) different-weight goal-tower-weight)
		      (- goal-tower-weight children-weights-sum))))
	      (loop
		 for subtree in (aoc-tree-children aoct)
		 for correction = (find-first-correction subtree)
		 while (not correction)
		 finally (return correction)))))))




