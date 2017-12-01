;;;; day2a.asd

(asdf:defsystem #:day2a
  :description "Describe day2a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day2a")))

