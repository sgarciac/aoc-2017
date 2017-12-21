;;;; day19a.lisp
(in-package #:day19b)

(defvar +up+ '(0 . 1))
(defvar +down+ '(0 . -1))
(defvar +left+ '(-1 . 0))
(defvar +right+ '(1 . 0))

(defun grid-val (grid x y)
  (let ((dims (array-dimensions grid)))
    (if (or
	 (< x 0) (< y 0) (>= x (first dims)) (>= y (second dims)))
	#\Space
	(aref grid x y))))

(defun next-x-in-direction (col direction)
  (+ col (car direction)))

(defun next-y-in-direction (row direction)
  (+ row (cdr direction)))

(defun next-direction(grid col row last-direction)
  (let ((next-col (next-x-in-direction col last-direction))
	(next-row (next-y-in-direction row last-direction)))
    (format t "next-direction: ~A,~A ~A~%" next-col next-row (grid-val grid next-col next-row))
    (if (char= #\+ (grid-val grid
			 next-col
			 next-row))
      (cond 
	((or (equal last-direction +up+) (equal last-direction +down+))
	 (print "VERTICAL")
	 (let ((left (grid-val grid
			      (next-x-in-direction next-col +left+)
			      next-row))
	       (right (grid-val grid
			      (next-x-in-direction next-col +right+)
			      next-row)))
	   (cond ((or (char= left #\-)
		      (char= left #\+)
		      (alphanumericp left))
		  +left+)
		 ((or (char= right #\-)
		      (char= right #\+)
		      (alphanumericp right))
		  +right+)
		 (t :END))))
	((or (equal last-direction +left+) (equal last-direction +right+))
	 (print "HORIZONTAL")
	 (let ((up (grid-val grid
			 next-col
			 (next-y-in-direction next-row +up+)))
	       (down (grid-val grid
			   next-col
			   (next-y-in-direction next-row +down+)
			   )))
	   (format t "up: ~A down ~A~%" up down)
	   (cond ((or (char= up #\|)
		      (char= up #\+)
		      (alphanumericp up))
		  (print "UP")
		  +up+)
		 ((or (char= down #\|)
		      (char= down #\+)
		      (alphanumericp down))
		  (print "DOWN")
		  +down+)
		 (t :END))))
	(t (error "eh?")))
      (let ((new-char (grid-val grid
			 (next-x-in-direction next-col last-direction)
			 (next-y-in-direction next-row last-direction))))
	(if (char= new-char #\Space)
	    :END
	    last-direction)))))
	 
(defun find-start-col(grid)
  (let ((dims (array-dimensions grid)))
    (loop for i from 0 below (car dims)
       while (not (char= (aref grid i (1- (cadr dims))) #\|))
	 finally (return i))))

(defun read-input (file)
  (let ((dims (with-open-file (in file)
		(loop
		   for line = (read-line in nil)
		   while line
		   collecting line into lines
		   finally (return (list (length (first lines))
					 (length lines)))))))
    (print dims)
    (with-open-file (in file)
      (loop
	 with grid = (make-array dims)
	 for line = (read-line in nil)
	 for row = (1- (second dims)) then (1- row)
	 while line
	 do (loop for col from 0 below (first dims)
		 do (setf (aref grid col row) (char line col)))
	 finally (return grid)))))


(defun day19b(grid)
  (let* (
       (dims (array-dimensions grid))
       (start-col (find-start-col grid))
       (start-row (1- (second dims))))
  (loop
     for direction = +down+ then (next-direction grid x y direction) ;
     and x = start-col then (next-x-in-direction x direction)
     and y = start-row then (next-y-in-direction y direction)
     while (not (eq direction :END))
     ;;do (format t "~A,~A (~A) ~A~%" x y (grid-val grid x y) direction)
     collecting (grid-val grid x y) into path
   
     finally (return (1+ (length path))))))
  
