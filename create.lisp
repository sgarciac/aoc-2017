(require 'quickproject)
(setf proj "day17b")

(quickproject:make-project proj :name proj :depends-on '(alexandria cl-ppcre) :author "sergio.garcia@gmail.com" :license "artistic license")
