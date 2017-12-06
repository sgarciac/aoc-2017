;;;; day6a.asd

(asdf:defsystem #:day6a
  :description "Describe day6a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day6a")))

