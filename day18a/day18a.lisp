;;;; day18a.lisp

(in-package #:day18a)

(defstruct inst
  inst
  arg1
  arg2)

(defun parse-line(line)
  (let ((parts (mapcar #'read-from-string (cl-ppcre:split " " line))))
    (if (= 2 (length parts))
	(make-inst
	 :inst (car parts)
	 :arg1 (cadr parts)
	 :arg2 nil)
	(make-inst
	 :inst (car parts)
	 :arg1 (cadr parts)
	 :arg2 (caddr parts)))))

(defun read-input (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line collect (parse-line line) into instructions
	 finally (return (make-array (length instructions) :initial-contents instructions)))))

(read-input "input")

(defun get-arg-val (arg regs)
  (if (numberp arg)
      arg
      (gethash arg regs)))

(defun execute-program(instructions)
  (loop
     with regs = (make-hash-table)
     with last-played = nil
     with last-received = nil
     with pc = 0 
     while (< pc (length instructions))
     do (let ((inst (aref instructions pc)))
	  (case (car inst)
	    ('SET (setf (gethash (inst-arg1 inst)) (get-arg-val (inst-arg2 inst) regs)))
	    ('LALA 1)))))


;(read-input "input")

