;;;; day11b.asd

(asdf:defsystem #:day11b
  :description "Describe day11b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day11b")))

