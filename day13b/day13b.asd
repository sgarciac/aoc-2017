;;;; day13b.asd

(asdf:defsystem #:day13b
  :description "Describe day13b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day13b")))

