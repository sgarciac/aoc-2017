;;;; day24a.asd

(asdf:defsystem #:day24a
  :description "Describe day24a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clem)
  :serial t
  :components ((:file "package")
               (:file "day24a")))

