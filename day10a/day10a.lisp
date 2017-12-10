;;;; day10a.lisp

(in-package #:day10a)

(setf common-lisp:*print-circle* t)

(defun read-input (file)
  (with-open-file (in file)
    (mapcar #'parse-integer (cl-ppcre:split "," (read-line in)))))

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

(defun day10a (input size)
  (loop 
     for circle = (make-initial-circle size) then (knot circle n)
     for n in input
     finally (return
               (let* ((c (circle-current circle))
                      (total-moved (+ (apply #'+ input)
                                      (/ (* (1- (length input)) (length input)) 2)))
                      (adjusted-list (nthcdr (- size (mod total-moved size)) c))
                      )
                 (* (first adjusted-list) (second adjusted-list))
                 ))))
