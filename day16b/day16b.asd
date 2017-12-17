;;;; day16b.asd

(asdf:defsystem #:day16b
  :description "Describe day16b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day16b")))

