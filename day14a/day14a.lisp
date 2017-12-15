;;;; day14a.lisp

(in-package #:day14a)

(setf common-lisp:*print-circle* t)

(defun read-input (file n)
  (with-open-file (in file)
    (let ((string (format nil "~A-~A" (read-line in) n)))
      (loop for c across string collect (char-code c)))))

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

(defun knot-hash (input)
  (let ((sh (sparse-hash input 256)))
    (loop for i from 0 upto 255 by 16
       collect (apply #'logxor (subseq sh i (+ i 16))))))

(defun get-bit-knot-hash (kh i)
  (multiple-value-bind (q r) (floor i 8)
    (ldb (byte 1 (- 7 r)) (nth q kh))))

(defun read-hashes(file)
  (loop
     for row from 0 below 128
     for knot-hash = (knot-hash (read-input file row))
     collecting knot-hash into hashes
     finally (return (make-array 128 :initial-contents hashes))))

(defun read-bit (hashes row col)
  (get-bit-knot-hash (aref hashes row) col))

(defun day14a(file)
  (let ((hashes (read-hashes file)))
    (loop for i below 128
       summing (loop for j below 128
		  summing (read-bit hashes i j)))))
  
(defun init-region-map(hashes)
  (loop with region-map = (make-array '(128 128))
       for row below 128
       do (loop for col below 128
	     do (setf (aref region-map row col)
		      (if (zerop (read-bit hashes row col))
			  0 ;; unused
			  -1 ;; used but unknown region
			  )))
       finally (return region-map)))

(defun neighboors-to-flag(region-map cell)
  (let ((directions '((0 . -1) (1 . 0) (0 . 1) (-1 . 0))))
    (when (zerop (car cell))
      (setf directions (remove '(-1 . 0) directions :test #'equal)))
    (when (= (car cell) 127)
      (setf directions (remove '(1 . 0) directions :test #'equal)))
    (when (zerop (cdr cell))
      (setf directions (remove '(0 . -1) directions :test #'equal)))
    (when (= (cdr cell) 127)
      (setf directions (remove '(0 . 1) directions :test #'equal)))
    (loop for candidate in (mapcar (lambda (dir) (cons (+ (car cell) (car dir))
				(+ (cdr cell) (cdr dir))))
			      directions)
	 when (= -1 (aref region-map (car candidate) (cdr candidate))) collect candidate
	 
	 )))

(defun flag-region (region-map cells-to-flag id)
  (loop
     while cells-to-flag
     do (let ((cell (pop cells-to-flag)))
	  (setf (aref region-map (car cell) (cdr cell)) id)
	  (setf cells-to-flag (union cells-to-flag (neighboors-to-flag region-map cell) :test #'equal)))))

(defun flag-regions(hashes)
  (loop
     with region-map = (init-region-map hashes)
     with current-group = 1
     for row below 128
     do (loop for col below 128
	   do (when (= -1 (aref region-map row col))
		(flag-region region-map (list (cons row col)) current-group)
		(incf current-group)))
       finally (return current-group)))




(flag-regions (read-hashes "input"))
