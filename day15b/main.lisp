(in-package #:day15b)

(time (loop
	 with a-generator = (make-generator-2 679 16807 4)
	 with b-generator = (make-generator-2 771 48271 8)
	 repeat 5000000
	 for a = (funcall a-generator)
	 for b = (funcall b-generator)
	 counting (match-lower16 a b)))
