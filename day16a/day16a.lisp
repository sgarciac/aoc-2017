;;;; day16a.lisp
(in-package #:day16a)

(defparameter *size* 16)

(defvar *progs*)
(defvar *prog-to-pos*)
(defvar *pos-to-prog*)

(defstruct instruction
  type
  arg1
  arg2
  )

(defmacro with-programs (size &rest body)
  `(let* ((*size* ,size)
          (*prog-to-pos* (make-array (list *size*)))
          (*pos-to-prog* (make-array (list *size*))))
     (loop for i below *size*
        do (progn
             (setf (aref *prog-to-pos* i) i)
             (setf (aref *pos-to-prog* i) i)))
     ,@body))

(defun char-to-number (char)
  (- (char-code char) 97))

(defun number-to-char (number)
  (code-char (+ number 97)))

(defun parse-instruction (string)
  (let* ((type (case (char string 0)
                 (#\s :SPIN)
                 (#\x :EXCHANGE)
                 (#\p :PARTNER)))
         (items (cl-ppcre:split "/" (subseq string 1))))
    (make-instruction
     :type type
     :arg1 (case type
             ((:SPIN :EXCHANGE) (parse-integer (first items)))
             (:PARTNER (char-to-number (char (first items) 0))))
     :arg2 (case type
             (:SPIN nil)
             (:EXCHANGE (parse-integer (second items)))
             (:PARTNER (char-to-number (char (second items) 0)))))))

(defun process-spin (suffix-size)
  (let ((prefix-size (- *size* suffix-size)))
    (loop for i below prefix-size
       do (setf (aref *prog-to-pos* (aref *pos-to-prog* i)) (+ i suffix-size)))
    (loop for i from prefix-size below *size*
       do (setf (aref *prog-to-pos* (aref *pos-to-prog* i)) (- i prefix-size)))
    (loop for i below *size*
       do (setf (aref *pos-to-prog* (aref *prog-to-pos* i)) i))))

(defun process-exchange (pos1 pos2)
  (let ((prog1 (aref *pos-to-prog* pos1))
        (prog2 (aref *pos-to-prog* pos2)))
    (setf (aref *pos-to-prog* pos1) prog2)
    (setf (aref *pos-to-prog* pos2) prog1)
    (setf (aref *prog-to-pos* prog1) pos2)
    (setf (aref *prog-to-pos* prog2) pos1)))

(defun process-partner (pg1 pg2)
  (let ((pos1 (aref *prog-to-pos* pg1))
        (pos2 (aref *prog-to-pos* pg2)))
    (setf (aref *pos-to-prog* pos1) pg2)
    (setf (aref *pos-to-prog* pos2) pg1)
    (setf (aref *prog-to-pos* pg1) pos2)
    (setf (aref *prog-to-pos* pg2) pos1)))

(defun process-instruction (instruction)
  (case (instruction-type instruction)
    (:SPIN (process-spin
            (instruction-arg1 instruction)))
    (:EXCHANGE (process-exchange
                (instruction-arg1 instruction)
                (instruction-arg2 instruction)
                ))
    (:PARTNER (process-partner
               (instruction-arg1 instruction)
               (instruction-arg2 instruction)
               ))))


(defun read-input (file)
  (with-open-file (in file)
    (loop for instruction in (cl-ppcre:split "," (read-line in))
       collecting (parse-instruction instruction))))

(defun prog-hash ()
  (loop for i below *size*
     summing (* (1+ (aref *pos-to-prog* i)) (expt 2 i))))


(defun program-chars ()
  (loop for i below *size*
     collect (number-to-char (aref *pos-to-prog* i)) into chars
     finally (return (apply #'concatenate 'string (list chars)))))

(defun day16a ()
  (with-programs 16
    (loop for instruction in (read-input "input")
       do (process-instruction instruction))
    (program-chars)))
