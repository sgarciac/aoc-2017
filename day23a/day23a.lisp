;;;; day23a.lisp

(in-package #:day23a)

(defvar *count* 0)
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

(defun read-instructions (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line collect (parse-line line))))


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
	  (case (inst-inst inst)
            (SET
             (setf (gethash (inst-arg1 inst) regs)
                   (get-arg-val (inst-arg2 inst) regs))
             (incf pc))
            (SUB
             (setf (gethash (inst-arg1 inst) regs)
                   (- (get-arg-val (inst-arg1 inst) regs)
                      (get-arg-val (inst-arg2 inst) regs)))
             (incf pc))
            (MUL
             (incf *count*)
             (setf (gethash (inst-arg1 inst) regs)
                   (* (get-arg-val (inst-arg1 inst) regs)
                      (get-arg-val (inst-arg2 inst) regs)))
             (incf pc))
            (JNZ
             (if (not (zerop (get-arg-val (inst-arg1 inst) regs)))
                 (incf pc (get-arg-val (inst-arg2 inst) regs))
                 (incf pc)
                 ))))))

(defun pepe (instructions)
  (macrolet ((compile-advent ()
               `(+ ,@(instructions))))
    (compile-advent)))

(pepe '(1 2 3))
(pepe (read-instructions "input"))

(defmacro compile-aoc (file)
  (let ((instructions (read-instructions file)))
    `(let (,@(loop for reg in '(A B C D E F G H)
                collect (list reg 0)
                  ))
       (declare (type fixnum A B C D E F G H))
       (setf A 1)
       (tagbody
          ,@(loop for inst in instructions
               for line = 0 then (1+ line)
               appending
                 (case (inst-inst inst)
                   (SET
                    `(,line (setf ,(inst-arg1 inst) ,(inst-arg2 inst))))
                   (SUB
                    `(,line (decf ,(inst-arg1 inst) ,(inst-arg2 inst))))
                   (MUL
                    `(,line (setf ,(inst-arg1 inst) (* ,(inst-arg1 inst)
                                                       ,(inst-arg2 inst)))))
                   (JNZ
                    `(,line (when (not (zerop ,(inst-arg1 inst)))
                              (go ,(+ line (inst-arg2 inst)))
                              )))) into code
               finally (return  (append code (list (1+ line))))

                 ))
       (list A B C D E F G H))))

(time (compile-aoc "input"))

(compile-aoc "input")

(defun execute-program2(instructions)
  (let ((regs (make-hash-table)))
    (setf (gethash 'A regs) 1)
    (loop
       with pc = 0
       while (< pc (length instructions))
       do (let ((inst (aref instructions pc)))
            (print pc)
            (case (inst-inst inst)
              (SET
               (setf (gethash (inst-arg1 inst) regs)
                     (get-arg-val (inst-arg2 inst) regs))
               (incf pc))
              (SUB
               (setf (gethash (inst-arg1 inst) regs)
                     (- (get-arg-val (inst-arg1 inst) regs)
                        (get-arg-val (inst-arg2 inst) regs)))
               (incf pc))
              (MUL
               (incf *count*)
               (setf (gethash (inst-arg1 inst) regs)
                     (* (get-arg-val (inst-arg1 inst) regs)
                        (get-arg-val (inst-arg2 inst) regs)))
               (incf pc))
              (JNZ
               (if (not (zerop (get-arg-val (inst-arg1 inst) regs)))
                   (incf pc (get-arg-val (inst-arg2 inst) regs))
                   (incf pc)
                   )))))))

                                        ;
(time (let ((*count* 0))  (execute-program (read-input "input")) *count*))






