;;;; day13a.asd

(asdf:defsystem #:day13a
  :description "Describe day13a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day13a")))

