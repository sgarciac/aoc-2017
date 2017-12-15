(require 'quickproject)
(setf proj "day15a")

(quickproject:make-project proj :name proj :depends-on '(alexandria cl-ppcre) :author "sergio.garcia@gmail.com" :license "artistic license")
