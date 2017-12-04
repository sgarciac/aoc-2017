(require 'quickproject)
(setf proj "day4b")

(quickproject:make-project proj :name proj :depends-on '(alexandria) :author "sergio.garcia@gmail.com" :license "artistic license")
