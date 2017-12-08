;;;; day8b.asd

(asdf:defsystem #:day8b
  :description "Describe day8b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day8b")))

