;;;; day20a.asd

(asdf:defsystem #:day20a
  :description "Describe day20a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day20a")))

