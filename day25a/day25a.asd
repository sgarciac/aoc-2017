;;;; day25a.asd

(asdf:defsystem #:day25a
  :description "Describe day25a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clem)
  :serial t
  :components ((:file "package")
               (:file "day25a")))

