;;;; day21a.lisp

(in-package #:day21a)

(defun hflip (m &optional (copy t))
  (let ((cp (if copy (copy-to-bit-matrix m) m)))
    (loop for i below (floor (/ (cols cp) 2))
       do (swap-cols cp i (- (cols cp) i 1))
       finally (return cp))))


(defun rotate (m &optional (copy t))
  (let ((cp (if copy (copy-to-bit-matrix m) m)))
    (hflip (transpose cp) nil)))

(defun serialize-matrix (m)
  (loop
     with s = (make-string-output-stream)
     for i below (rows m)
     do (loop for j below (cols m)
           do (format s "~[.~;#~]" (val m i j ))
           finally (unless (= i (1- (rows m)))
                     (format s "/" )
                     ))
     finally (return (get-output-stream-string s))))

(defun matrix-id (m)
  (intern (serialize-matrix m)))

(defun trans-ids (m)
  (let* ((t0 m)
         (t1 (rotate t0))
         (t2 (rotate t1))
         (t3 (rotate t2))
         (t4 (hflip t0))
         (t5 (hflip t1))
         (t6 (hflip t2))
         (t7 (hflip t3)))
    (remove-duplicates (mapcar #'matrix-id (list t0 t1 t2 t3 t4 t5 t6 t7)))))

(defun parse-matrix (line)
  (let* ((items (cl-ppcre:split "/" line))
         (m (make-instance 'bit-matrix
                           :cols (length (first items))
                           :rows (length items)
                           )))
    (loop for line in items
       for row = 0 then (1+ row)
       do (loop
             for c across line
             for col = 0 then (1+ col)
             do (set-val m row col (if (char= #\. c) 0 1))))
    m))

(defun read-input (file)
  (with-open-file (in file)
    (loop
       with manual = (make-hash-table)
       for line = (read-line in nil)
       while line collect (let* ((parts (cl-ppcre:split " => " line))
                                 (input (parse-matrix (first parts)))
                                 (output (parse-matrix (second parts))))
                            (loop for id in (trans-ids input)
                               do (setf (gethash id manual) output)))
       finally (return manual))))


(defun next-matrix (m manual)
  (let* ((segment (if (evenp (rows m)) 2 3))
         (count (/ (rows m) segment)))
    (loop
       for nr below count
       collecting
         (loop for
            nc below count
            collecting
              (let
                  ((submatrix
                    (subset-matrix 
                     m
                     (* segment nr)
                     (1- (* segment (1+ nr)))
                     (* segment nc)
                     (1- (* segment (1+ nc))))))
                (gethash (matrix-id submatrix) manual))
              ) into nrows
       finally (return
                 (apply #'vertcat (mapcar (lambda (nrow) (apply #'horzcat nrow)) nrows))
                 ))))

(defun count-ones (m)
  (loop
     for i below (rows m)
     summing (loop for j below (cols m)
                when (= 1 (val m i j)) summing 1 )))

(defun day21a (init manual iterations)
  (loop
     for i below (1+ iterations)
     for m = init then (next-matrix m manual)
     do (print i)
     finally (return m)))
