(require 'quickproject)
(setf proj "day6b")

(quickproject:make-project proj :name proj :depends-on '(alexandria) :author "sergio.garcia@gmail.com" :license "artistic license")
