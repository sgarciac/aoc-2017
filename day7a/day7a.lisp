;;;; day7a.lisp
(in-package #:day7a)

(defstruct input-entry
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

(defun find-base-name (parents)
  (loop for key being the hash-keys of parents
     do (when (not (gethash key parents)) (return key))))





