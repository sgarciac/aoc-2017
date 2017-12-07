;;;; day7a.asd

(asdf:defsystem #:day7a
  :description "Describe day7a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day7a")))

