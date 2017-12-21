;;;; day20b.asd

(asdf:defsystem #:day20b
  :description "Describe day20b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day20b")))

