(in-package #:day18b)

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

(defstruct (queue (:constructor %make-queue (xs &aux (ys (last xs)))))
  (head xs :read-only t)
  (tail ys :read-only t))

(defun make-queue (&key initial-contents)
  "Create a new queue."
  (%make-queue (cons nil initial-contents)))

(defmethod print-object ((q queue) stream)
  "Output a deque to a stream."
  (print-unreadable-object (q stream :type t)
    (format stream "~:[EMPTY~;~:*~a~]" (rest (queue-head q)))))

(defun queue-push (x q)
  "Push a value onto the tail of the queue."
  (with-slots (tail)
      q
    (first (setf tail (cdr (rplacd tail (list x)))))))

(defun queue-pop (q)
  "Pop a value off a queue, return the value and success flag."
  (with-slots (head)
      q
    (when (rest head)
      (first (setf head (rest head))))))

(defun queue-empty-p (q)
  (with-slots (head)
      q
    (not (rest head))))

(defun print-program (regs pc)
  (format t "PC: ~A --- " pc)
  (maphash (lambda (k v) (format t "~A:~A , " k v)) regs)
  (format t "~%"))

(defun generate-instance(id instructions write-queue read-queue)
  (let ((pc 0)
        (regs (make-hash-table)))
    (setf (gethash 'P regs) id)
    (lambda ()
      (block aoc-program
        (loop
           while (< pc (length instructions))
           do (let ((inst (aref instructions pc)))
                (case (inst-inst inst)
                  (SND
                   (queue-push (get-arg-val (inst-arg1 inst) regs) write-queue)
                   (incf pc)
                   (return-from aoc-program :SENT))
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
                   (let ((received (queue-pop read-queue)))
                     (cond (received
                            (setf (gethash (inst-arg1 inst) regs) received)
                            (incf pc)
                            (return-from aoc-program :RECEIVED))
                           (t
                            (print-program regs pc)
                            (return-from aoc-program :WAITING)))))
                  (JGZ
                   (if (> (get-arg-val (inst-arg1 inst) regs) 0)
                       (incf pc (get-arg-val (inst-arg2 inst) regs))
                       (incf pc)
                       )))))
        :FINISHED))))

(defun day18b (input)
  (let* ((queue1 (make-queue))
       (queue2 (make-queue))
       (program1 (generate-instance 0 input queue1 queue2))
       (program2 (generate-instance 1 input queue2 queue1)))
  (loop
     with program1sent = 0
     with program1received = 0
     with program2sent = 0
     with program2received = 0
     with status1 = :INIT
     with status2 = :INIT
     while (not
            (or 
             (and (eq status1 :FINISHED) (eq status2 :FINISHED))
             (and (eq status1 :FINISHED) (and (eq status2 :WAITING) (queue-empty-p queue1)))
             (and (eq status2 :FINISHED) (and (eq status1 :WAITING) (queue-empty-p queue2)))
             (and (eq status1 :WAITING) (eq status2 :WAITING) (queue-empty-p queue1) (queue-empty-p queue2))))
     do (progn
          (loop
             while (not
                    (or
                     (eq status1 :FINISHED)
                     (and (eq status1 :WAITING) (queue-empty-p queue2))
                     )) 
             do (let ((status (funcall program1)))
                  (when (eq status :SENT) (incf program1sent))
                  (when (eq status :RECEIVED) (incf program1received))
                  (setf status1 status)))
          (loop
             while (not
                    (or
                     (eq status2 :FINISHED)
                     (and (eq status2 :WAITING) (queue-empty-p queue1))
                     )) 
             do (let ((status (funcall program2)))
                  (when (eq status :SENT) (incf program2sent))
                  (when (eq status :RECEIVED) (incf program2received))
                  (setf status2 status)))
          (format t "-------------------------------~%")
          (format t "p1: ~A ~A ~A~%" program1sent program1received status1)
          (format t "p2: ~A ~A ~A~%" program2sent program2received status2)
          ;;(when (eq 'Q (read)) (error "finished"))
          )
     finally (return program2sent)
       )))
