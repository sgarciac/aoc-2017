;;;; day3a.lisp

(in-package #:day3a)

(defun square-side-size-for-location (pos)
  "returns the size of the square to which the number 'pos' belongs"
  (let ((sqsr (ceiling (sqrt pos))))
    (if (oddp sqsr)
        sqsr
        (1+ sqsr))))

(defun square-position (pos)
  "Calculate the position in its square for number pos, from 0 at
the right-middle position, starting at the directly upper position from right bottom corner, and counting anti-clockwise. Returns position and square size"
  (let* ((ss (square-side-size-for-location pos))
         (sp (- pos
                (expt (- ss 2) 2)
                (/ (1- ss) 2))))
    (values sp ss)))

(defun day3a (pos)
  (if (= 1 pos)
      0 
      (multiple-value-bind (sp ss) (square-position pos)
        (+ (/ (1- ss) 2)
           (let ((steps (1- ss)))
             (- (/ steps 2)
                (abs (- (/ steps 2)
                        (mod sp steps)))))))))
