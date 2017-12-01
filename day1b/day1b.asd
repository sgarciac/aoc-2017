;;;; day1b.asd

(asdf:defsystem #:day1b
  :description "day1b"
  :author "Sergio Garcia <sergio.garcia@gmail.com>"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day1b")))

