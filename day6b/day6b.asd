;;;; day6b.asd

(asdf:defsystem #:day6b
  :description "Describe day6b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day6b")))

