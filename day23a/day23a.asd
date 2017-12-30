;;;; day23a.asd

(asdf:defsystem #:day23a
  :description "Describe day23a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clem)
  :serial t
  :components ((:file "package")
               (:file "day23a")))

