(in-package #:day1)

(defun read-input (file)
  (with-open-file (stream file)
	 (read-line stream)))

(defun calculate-sum (input)
  (labels ((char-to-int (char)
	     (- (char-int char) 48))
	   (next-index (index length)
	     (if (= (1- length) index)
		 0
		 (1+ index))))
    (loop
       with length = (length input)
       for i below length
       for j = (next-index i length) then (next-index i length) 
       when (char= (aref input i) (aref input j)) sum (char-to-int (aref input i)))))
