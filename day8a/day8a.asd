;;;; day8a.asd

(asdf:defsystem #:day8a
  :description "Describe day8a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day8a")))

