;;;; day2b.asd

(asdf:defsystem #:day2b
  :description "Describe day2b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day2b")))

