;;;; day3a.asd

(asdf:defsystem #:day3a
  :description "Describe day3a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day3a")))

