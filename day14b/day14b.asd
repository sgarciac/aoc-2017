;;;; day14b.asd

(asdf:defsystem #:day14b
  :description "Describe day14b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day14b")))

