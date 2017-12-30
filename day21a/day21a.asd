;;;; day21a.asd

(asdf:defsystem #:day21a
  :description "Describe day21a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clem)
  :serial t
  :components ((:file "package")
               (:file "day21a")))

