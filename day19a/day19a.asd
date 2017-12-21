;;;; day19a.asd

(asdf:defsystem #:day19a
  :description "Describe day19a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day19a")))

