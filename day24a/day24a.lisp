;;;; day24a.lisp

(in-package #:day24a)

(defun read-input (file)
  (with-open-file (in file)
    (loop
       for line = (read-line in nil)
       while line collect (mapcar #'parse-integer (cl-ppcre:split "/" line)))))

(defun bridge-value (links)
  (loop for link in links
     summing (+ (first link) (second link))))

(defun max-bridge (start links)
  (if links
      (let ((match-links (remove-if (lambda (link) (not (find start link))) links)))
        (if match-links
            (loop
               with max-chain = '()
               for candidate in match-links
               do (let* ((other-pole (if (= (first candidate) (second candidate)) (first candidate) (find-if (lambda (v) (not (= v start))) candidate)))
                         (other-links (remove candidate links :test #'equal))
                         (next-chain (max-bridge other-pole other-links )))
                    (when (> (bridge-value (cons candidate next-chain)) (bridge-value max-chain))
                      (setf max-chain (cons candidate next-chain))))
               finally (return max-chain))
            nil
            ))
      nil
      ))


(defun max-bridge3 (start links)
  (if links
      (let ((match-links (remove-if (lambda (link) (not (find start link))) links)))
        (if match-links
            (loop
               with max-chain = :START
               for candidate in match-links
               do (let* ((other-pole
                          (if (= (first candidate) (second candidate))
                              (first candidate)
                              (find-if (lambda (v) (not (= v start))) candidate)))
                         (other-links (remove candidate links :test #'equal))
                         (next-chain (max-bridge3 other-pole other-links )))
                                        ;(format t "testing candidate ~A ~%" (cons candidate next-chain))
                    (cond ((or (eq max-chain :START) (> (1+ (length next-chain)) (length max-chain)))
                                        ;(format t "comparing ~A ~A~%" next-chain max-chain)
                           (setf max-chain (cons candidate next-chain))
                           )
                          ((and  (= (1+ (length next-chain)) (length max-chain))
                                 (> (bridge-value (cons candidate next-chain))
                                    (bridge-value max-chain)))
                           (setf max-chain (cons candidate next-chain))
                           ) ))
               finally (return max-chain))
            nil
            ))
      nil
      ))

(bridge-value  (max-bridge3 0 (read-input "input")))

(let ((a 1))
  (cond ((= a 1)
         (print "hello")
         (setf a 2))
        ((= a 2)
         (print "world")))
  a)


