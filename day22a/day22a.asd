;;;; day22a.asd

(asdf:defsystem #:day22a
  :description "Describe day22a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clem)
  :serial t
  :components ((:file "package")
               (:file "day22a")))

