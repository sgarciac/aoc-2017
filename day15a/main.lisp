(in-package #:day15a)

(time (loop
   with a-generator = (make-generator 679 16807)
   with b-generator = (make-generator 771 48271)
   repeat 40000000
   for a = (funcall a-generator)
   for b = (funcall b-generator)
   counting (match-lower16 a b)))
