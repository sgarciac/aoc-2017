;;;; day15a.asd

(asdf:defsystem #:day15a
  :description "Describe day15a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day15a")))

