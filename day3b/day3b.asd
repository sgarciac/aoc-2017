;;;; day3b.asd

(asdf:defsystem #:day3b
  :description "Describe day3b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day3b")))

