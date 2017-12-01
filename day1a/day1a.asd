;;;; day1a.asd

(asdf:defsystem #:day1a
  :description "day1a"
  :author "Sergio Garcia <sergio.garcia@gmail.com>"
  :license "artistic license"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "day1a")))

