;;;; day15b.asd

(asdf:defsystem #:day15b
  :description "Describe day15b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day15b")))

