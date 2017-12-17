;;;; day17a.asd

(asdf:defsystem #:day17a
  :description "Describe day17a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day17a")))

