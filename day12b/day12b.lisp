;;;; day12b.lisp

(in-package #:day12b)

(defvar *programs* '())
(defvar *destinations* (make-hash-table))
(defvar *paths* (make-hash-table :test 'equal))

(defun add-path (org dest)
  (setf *programs* (adjoin org *programs*))
  (setf *programs* (adjoin dest *programs*))
  (setf
   (gethash org *destinations*)
   (adjoin dest (gethash org *destinations*)))
  (setf
   (gethash (cons org dest) *paths*)
   t))

(defun has-path (org dest)
  (gethash (cons org dest) *paths*))

(defun get-destinations (org)
  (gethash org *destinations*))

(defmacro with-graph (&rest body)
  `(let ((*programs* '())
	 (*destinations* (make-hash-table))
	 (*paths* (make-hash-table :test 'equal)))
     ,@body))

(defun process-line (line)
  (let* ((items (remove "" (cl-ppcre:split "[, ]" line) :test #'string-equal))
	 (org (parse-integer (first items)))
	 (dests (mapcar #'parse-integer (cddr items))))
    (loop for dest in dests
       do (progn (add-path org dest)
		 (add-path dest org)))))

(defun load-graph (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line do (process-line line))))

(defun group(org)
  (loop
     with visited = '()
     with to-visit = (list org)
     while to-visit
     do (let ((next (car to-visit)))
	  (setf to-visit (cdr to-visit))
	  (setf visited (adjoin next visited))
	  (setf to-visit (append to-visit (set-difference (set-difference (get-destinations next) to-visit) visited))))
       finally (return visited)))


(defun groups()
  (loop
     with programs = *programs*
     with groups = '()
     while programs
     do (let* ((next (car programs))
	       (group (group next)))
	  (setf groups (cons group groups))
	  (setf programs (set-difference programs group)))
     finally (return groups)))


