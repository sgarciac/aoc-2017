;;;; day22a.lisp

(in-package #:day22a)

;; sparse matrix
(defvar *sm*)
(defvar *cr*) ;;current row
(defvar *cc*) ;; current col
(defvar *dir-r*)
(defvar *dir-c*)
(defvar *count-set-sm*)

(defun set-sm (i j)
  (incf *count-set-sm*)
  (setf (gethash (cons i j) *sm* ) t))

(defun unset-sm (i j)
  (remhash (cons i j) *sm*))

(defun sm-setp (i j)
  (gethash (cons i j) *sm*))

(defun flip-sm (i j)
  (if  (sm-setp i j)
       (unset-sm i j)
       (set-sm i j)))


(defun coins ()
  (let ((points (alexandria:hash-table-keys *sm*)))
    (loop for point in points
       minimizing (car point) into minrow
       maximizing (car point) into maxrow
       minimizing (cdr point) into mincol
       maximizing (cdr point) into maxcol
       finally (return (values minrow maxrow mincol maxcol)))))

(defun infected-p ()
  (sm-setp *cr* *cc*))

(defun advance ()
  (let ((dir-r *dir-r*)
        (dir-c *dir-c*))
    (cond ((infected-p)
           (setf *dir-r* dir-c)
           (setf *dir-c* (* -1 dir-r)))
          (t
           (setf *dir-c* dir-r)
           (setf *dir-r* (* -1 dir-c))))
    (flip-sm *cr* *cc*)
    (incf *cr* *dir-r*)
    (incf *cc* *dir-c*)))

(defun print-sm ()
  (format t "~%")
  (multiple-value-bind (minrow maxrow mincol maxcol) (coins)
    (loop
       for i from (min *cr* minrow) upto (max maxrow *cr*)
       do (loop for j from (min *cc* mincol) upto (max  maxcol *cc*)
             do (format t "~[#~;.~;X~;O~]" (if (and (= i *cr*)
                                                    (= j *cc*))
                                               (if (sm-setp i j)
                                                   2
                                                   3)
                                               (if (sm-setp i j)
                                                   0
                                                   1)))
             finally (format t "~%")))))

(defun init-sm (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       for i = 0 then (1+ i)
       while line
       do (loop
             for c across line
             for j = 0 then (1+ j)
             do (when (char= c #\#) (set-sm i j)))
       finally (let ((mrow (floor i 2))
                     (mcol (floor i 2)))
                 (setf *count-set-sm* 0)
                 (setf *cr* mrow)
                 (setf *cc* mcol)
                 (setf *dir-r* -1)
                 (setf *dir-c* 0)))))

(defmacro with-clean-sm ((file) &body body)
  `(let ((*cr* 0)
         (*cc* 0)
         (*dir-r* 0)
         (*dir-c* 0)
         (*count-set-sm* 0)
         (*sm* (make-hash-table :test #'equal)))
     (init-sm ,file)
     ,@body))

(with-clean-sm ("input")
  (dotimes (i 10000)
    (advance))
  (print-sm)
  *count-set-sm*
  )



