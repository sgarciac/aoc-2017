;;;; day18b.asd

(asdf:defsystem #:day18b
  :description "Describe day18b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day18b")))

