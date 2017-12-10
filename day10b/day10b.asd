;;;; day10b.asd

(asdf:defsystem #:day10b
  :description "Describe day10b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day10b")))

