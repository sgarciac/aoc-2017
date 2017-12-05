;;;; day5a.asd

(asdf:defsystem #:day5a
  :description "Describe day5a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day5a")))

