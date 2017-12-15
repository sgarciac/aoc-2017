;;;; day14a.asd

(asdf:defsystem #:day14a
  :description "Describe day14a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day14a")))

