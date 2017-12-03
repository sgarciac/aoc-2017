;;;; day3b.lisp
(in-package #:day3b)
(defparameter *size* 99)

(defstruct location
  x
  y
  )

(defun left (location)
  (make-location :x (1- (location-x location))
                 :y (location-y location)))

(defun right (location)
  (make-location :x (1+ (location-x location))
                 :y (location-y location)))

(defun up (location)
  (make-location :x (location-x location)
                 :y (1+ (location-y location))))

(defun down (location)
  (make-location :x (location-x location)
                 :y (1- (location-y location))))

(defun left-up (location)
  (make-location :x (1- (location-x location))
                 :y (1+ (location-y location))))

(defun right-up (location)
  (make-location :x (1+ (location-x location))
                 :y (1+ (location-y location))))

(defun left-down (location)
  (make-location :x (1- (location-x location))
                 :y (1- (location-y location))))

(defun right-down (location)
  (make-location :x (1+ (location-x location))
                 :y (1- (location-y location))))

(defun square-side-size-for-location (pos)
  "returns the size of the square to which the number 'pos' belongs"
  (let ((sqsr (ceiling (sqrt pos))))
    (if (oddp sqsr)
        sqsr
        (1+ sqsr))))

(defun middle-square-location (side-size)
  (let ((middle (/ (1- side-size) 2)))
    (make-location :x middle :y middle)))

(defun set-location-val (matrix location val)
  (setf (aref matrix (location-x location) (location-y location)) val))

(defun make-matrix ()
  (let* ((ss *size*)
         (middle (middle-square-location ss))
         (matrix (make-array (list ss ss) :initial-element 0)))
    (set-location-val matrix middle 1)
    matrix))


(defun val-for-loc (matrix location)
  (aref matrix (location-x location) (location-y location)))

(defun next-location (location matrix)
  (cond ((and
          (not (zerop (val-for-loc matrix (left location))))
          (zerop (val-for-loc matrix (up location))))
         (up location))
        ((not (zerop (val-for-loc matrix (down location))))
         (left location))
        ((not (zerop (val-for-loc matrix (right location))))
         (down location))
        (t
         (right location))))

(defun fill-next (matrix location)
  (let* ((nl (next-location location matrix))
         (nv
          (+
           (val-for-loc matrix (up nl))
           (val-for-loc matrix (down nl))
           (val-for-loc matrix (left nl))
           (val-for-loc matrix (right nl))
           (val-for-loc matrix (left-up nl))
           (val-for-loc matrix (left-down nl))
           (val-for-loc matrix (right-up nl))
           (val-for-loc matrix (right-down nl)))))
    (set-location-val matrix nl nv)
    nl))

(defun day3b (target)
  (loop
     with matrix = (make-matrix)
     for location = (middle-square-location *size*) then (fill-next matrix location)
     while (<= (val-for-loc matrix location) target)
     finally (return (val-for-loc matrix location))))
