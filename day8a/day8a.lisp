;;;; day8a.lisp

(in-package #:day8a)

(defstruct instruction
  register
  type
  value
  cond-register
  cond-type
  cond-value)


(defun parse-instruction (line)
  (let ((items (remove "" (cl-ppcre:split " " (string-upcase line) :omit-unmatched-p t) :test #'string=)))
    (make-instruction
     :register (intern (first items))
     :type (intern (second items))
     :value (parse-integer (third items))
     :cond-register (intern (fifth items))
     :cond-type (intern (sixth items))
     :cond-value (parse-integer (seventh items))
     )))

(defun read-input(file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line collect (parse-instruction line))))

(defun read-reg (mem reg)
  (let ((val (gethash reg mem)))
    (unless val
      (setf (gethash reg mem) 0))
    (gethash reg mem)))

(defun inc-reg (mem reg val)
  (setf (gethash reg mem) (+ (read-reg mem reg) val)))

(defun get-lisp-condition(aoc-condition)
  (cond ((eq aoc-condition '>)
	 #'>)
	((eq aoc-condition '<)
	 #'<)
	((eq aoc-condition '>=)
	 #'>=)
	((eq aoc-condition '<=)
	 #'<=)
	((eq aoc-condition '==)
	 #'=)
	((eq aoc-condition '!=)
	 (lambda(x y) (not (= x y))))
	(t (error "unknown aoc cond"))))

(defun max-value (memory)
  (loop for value being the hash-values of memory
     maximizing value))

(defun print-memory (memory)
  (loop for key being the hash-keys of memory
     do (format t "~A = ~A~%" key (gethash key memory))))

(defun execute (mem instructions)
  (loop
     for instruction in instructions
       maximizing (max-value mem) into max-value-ever
     do (let ((value (* (instruction-value instruction)
			(if (eq (instruction-type instruction) 'INC) 1 -1)))
	      (cond-evaluation (funcall (get-lisp-condition (instruction-cond-type instruction))
					(read-reg mem (instruction-cond-register instruction))
					(instruction-cond-value instruction))))
	  (if cond-evaluation
	      (inc-reg mem (instruction-register instruction) value)))
       finally (return max-value-ever)))

(defun day8a(instructions)
  (let ((memory (make-hash-table)))
    (execute memory instructions)
    memory))



