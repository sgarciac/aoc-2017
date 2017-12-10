;;;; day10b.lisp

(in-package #:day10b)

(setf common-lisp:*print-circle* t)

(defun read-input (file)
  (with-open-file (in file)
    (loop for c across (read-line in) collect (char-code c))
    ))

(defstruct circle
  size
  current
  previous
  skip
  )

(defun create-circle (list list-size skip)
  (let ((last (last list)))
    (rplacd last list)
    (make-circle
     :current list
     :previous last
     :size list-size
     :skip skip)))

(defun skip-circle (circle)
  (make-circle
   :current (nthcdr (circle-skip circle) (circle-current circle))
   :previous (nthcdr (circle-skip circle) (circle-previous circle))
   :size (circle-size circle)
   :skip (+ 1 (circle-skip circle))))


(defun make-initial-circle (size)
  (create-circle (loop for n below size collect n) size 0))

(defun knot (circle length)
  (if (zerop length)
      (skip-circle circle)
      (let* ((c (circle-current circle))
             (next (nthcdr length c))
             (sublist-last (nthcdr (1- length) c)))
        (rplacd sublist-last nil)
        (let* ((reversed-sublist (reverse c))
               (last-reversed-sublist (last reversed-sublist))
               (new-circle (cond ((= (circle-size circle) length)
                                  (create-circle reversed-sublist
                                                 (circle-size circle)
                                                 (circle-skip circle)))
                                 (t
                                  (rplacd (circle-previous circle) reversed-sublist)
                                  (rplacd last-reversed-sublist next)
                                  (make-circle
                                   :current next
                                   :previous last-reversed-sublist
                                   :size (circle-size circle)
                                   :skip (circle-skip circle))))))
          (skip-circle new-circle)))))


(defun sparse-hash (input size)
  (loop
     with circle = (make-initial-circle size)
     with ainput = (append input '(17 31 73 47 23))
     repeat 64
     do (dolist (n ainput)
          (setf circle (knot circle n)))
     finally (return
               (let* ((c (circle-current circle))
                      (total-moved (+ ( * 64 (apply #'+ ainput)) 
                                      (/ (* (1- (* 64 (length ainput))) (* 64 (length ainput))) 2)))
                      (adjusted-list (nthcdr (- size (mod total-moved size)) c))
                      )
                 (rplacd (nthcdr (1- size) adjusted-list)  nil)
                 adjusted-list))))

(defun day10b (input)
  (let ((sh (sparse-hash input 256)))
    (loop for i from 0 upto 255 by 16
       collect (format nil "~2,'0x" (apply #'logxor (subseq sh i (+ i 16)))) into pieces
       finally  (return (string-downcase (apply #'concatenate 'string pieces ))))))
