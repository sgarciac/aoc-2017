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


(defun get-arg-val (arg regs)
  (if (numberp arg)
      arg
      (if (gethash arg regs)
          (gethash arg regs)
          (setf (gethash arg regs) 0))))

(defun execute-program(instructions)
  (loop
     with regs = (make-hash-table)
     with last-played = nil
     with last-received = nil
     with pc = 0
     while (< pc (length instructions))
     do (let ((inst (aref instructions pc)))
          (print pc)
	  (case (inst-inst inst)
            (SND
             (setf last-played (get-arg-val (inst-arg1 inst) regs))
             (incf pc))
            (SET
             (setf (gethash (inst-arg1 inst) regs)
                   (get-arg-val (inst-arg2 inst) regs))
             (incf pc))
            (ADD
             (setf (gethash (inst-arg1 inst) regs)
                   (+ (get-arg-val (inst-arg1 inst) regs)
                      (get-arg-val (inst-arg2 inst) regs)))
             (incf pc))
            (MUL
             (setf (gethash (inst-arg1 inst) regs)
                   (* (get-arg-val (inst-arg1 inst) regs)
                      (get-arg-val (inst-arg2 inst) regs)))
             (incf pc))
            (MOD
             (setf (gethash (inst-arg1 inst) regs)
                   (mod (get-arg-val (inst-arg1 inst) regs)
                        (get-arg-val (inst-arg2 inst) regs)))
             (incf pc))
            (RCV
             (when (not (zerop (get-arg-val (inst-arg1 inst) regs))) 
               (setf last-received last-played)
               (return-from execute-program last-received))
             (incf pc))
            (JGZ
             (if (> (get-arg-val (inst-arg1 inst) regs) 0)
                 (incf pc (get-arg-val (inst-arg2 inst) regs))
                 (incf pc)
                 )
             )))))

(execute-program (read-input "input"))
