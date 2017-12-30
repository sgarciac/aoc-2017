(require 'quickproject)
(setf proj "day25a")

(quickproject:make-project proj :name proj :depends-on '(alexandria cl-ppcre clem) :author "sergio.garcia@gmail.com" :license "artistic license")
