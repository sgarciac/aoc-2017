;;;; day19b.asd

(asdf:defsystem #:day19b
  :description "Describe day19b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "day19b")))

