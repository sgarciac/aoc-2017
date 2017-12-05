;;;; day5b.asd

(asdf:defsystem #:day5b
  :description "Describe day5b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day5b")))

