;;;; day12b.asd

(asdf:defsystem #:day12b
  :description "Describe day12b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day12b")))

