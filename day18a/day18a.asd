;;;; day18a.asd

(asdf:defsystem #:day18a
  :description "Describe day18a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day18a")))

