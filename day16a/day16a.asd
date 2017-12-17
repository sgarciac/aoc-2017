;;;; day16a.asd

(asdf:defsystem #:day16a
  :description "Describe day16a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day16a")))

