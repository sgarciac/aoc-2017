;;;; day25a.lisp

(in-package #:day25a)

;;; "day25a" goes here. Hacks and glory await!

(defvar *current-state*)
(defvar *current-cell*)
(defvar *current-machine*)
(defparameter *steps* 12399302)


(defparameter *day25-input*
  '(
    (:A . ((0 1 :RIGHT :B) (1 0 :RIGHT :C)))
    (:B . ((0 0 :LEFT :A) (1 0 :RIGHT :D)))
    (:C . ((0 1 :RIGHT :D) (1 1 :RIGHT :A)))
    (:D . ((0 1 :LEFT :E) (1 0 :LEFT :D)))
    (:E . ((0 1 :RIGHT :F) (1 1 :LEFT :B)))
    (:F . ((0 1 :RIGHT :A) (1 1 :RIGHT :E)))))

(defun current-cell-value ()
  (or (gethash *current-cell* *current-machine*) 0))

(defun set-current-cell-value (val)
  (if (= val 1)
      (setf (gethash *current-cell* *current-machine*) 1)
      (remhash *current-cell* *current-machine*)))

(defun execute-step ()
  (let* ((entry (cdr (assoc (current-cell-value) (cdr (assoc *current-state* *day25-input*))))))
    (set-current-cell-value (first entry))
    (if (eq (second entry) :RIGHT)
        (incf *current-cell*)
        (decf *current-cell*))
    (setf *current-state* (third entry))
    *current-state*
    ))

(defmacro with-machine (&body body)
  `(let ((*current-state* :A)
         (*current-cell* 0)
         (*current-machine* (make-hash-table)))
     ,@body))

(defun day25 ()
  (with-machine
    (loop repeat *steps*
       do (execute-step))
    (hash-table-count  *current-machine*)))



