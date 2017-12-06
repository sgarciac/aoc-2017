;;;; day6b.lisp

;;;; day6a.lisp

(in-package #:day6b)

(defun read-input (file)
  (with-open-file (in file)
    (loop for n = (read in nil)
       while n collect n into entries
       finally (return (make-array (length entries) :initial-contents entries)))))

(defun max-index (array)
  (loop
     for i from 0 below (length array)
     for max-index = 0 then (if (> (aref array i) (aref array max-index))
                                i
                                max-index)
     finally (return max-index)))

(defun step-memory (input)
  (let* ((memory (copy-seq input))
         (al (length memory))
         (mi (max-index memory))
         (q (aref memory mi))
         (small-share (floor q al))
         (big-share (1+ small-share))
         (big-share-n (mod q al)))
    (setf (aref memory mi) 0)
    (loop
       for n from 1 upto (min al q)
       for index = (mod (+ mi n) al)
       do (if (<= n big-share-n)
              (incf (aref memory index) big-share)
              (incf (aref memory index) small-share))
       finally (return memory))))

(defun day6b (input)
  (let ((known-steps (make-hash-table :test #'equalp)))
    (loop
       for count = 0 then (1+ count)
       for current = input then (step-memory current)
       while (not (gethash current known-steps))
       do (progn (print current) (setf (gethash current known-steps) count)) 
       finally (return (- count (gethash current known-steps))))))

