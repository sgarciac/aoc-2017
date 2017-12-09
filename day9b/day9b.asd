;;;; day9b.asd

(asdf:defsystem #:day9b
  :description "Describe day9b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day9b")))

