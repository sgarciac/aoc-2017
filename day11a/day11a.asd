;;;; day11a.asd

(asdf:defsystem #:day11a
  :description "Describe day11a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day11a")))

