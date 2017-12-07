;;;; day7b.asd

(asdf:defsystem #:day7b
  :description "Describe day7b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day7b")))

