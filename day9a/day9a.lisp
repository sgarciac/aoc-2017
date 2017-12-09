;;;; day9a.lisp
(in-package #:day9a)

(defvar *total-groups* 0)

(defun parse-garbage (in)
  (read-char in)
  (loop
     for nc = (peek-char t in)
     do (cond
          ((char= nc #\!)
           (read-char in)(read-char in))
          ((char= nc #\>)
           (read-char in) (return))
          (t
           (read-char in)))))

(defun parse-group (in level)
  (incf *total-groups* (1+ level))
  (read-char in (1+ level))
  (loop
     for nc = (peek-char t in)
     do (cond
          ((char= nc #\})
           (read-char in) (return))
          ((char= nc #\,)
           (read-char in))
          ((char= nc #\{)
           (parse-group in (1+ level)))
          ((char= nc #\<)
           (parse-garbage in)))))

(defun day9a (file)
  (let ((*total-groups* 0))
    (with-open-file (in file)
      (parse-group in 0))
    *total-groups*
    ))
