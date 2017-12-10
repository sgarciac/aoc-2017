;;;; day10a.asd

(asdf:defsystem #:day10a
  :description "Describe day10a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day10a")))

