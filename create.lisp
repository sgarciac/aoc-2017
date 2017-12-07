(require 'quickproject)
(setf proj "day7a")

(quickproject:make-project proj :name proj :depends-on '(alexandria split-sequence) :author "sergio.garcia@gmail.com" :license "artistic license")
