;;;; day12a.asd

(asdf:defsystem #:day12a
  :description "Describe day12a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day12a")))

