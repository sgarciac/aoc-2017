;;;; day17b.asd

(asdf:defsystem #:day17b
  :description "Describe day17b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day17b")))

