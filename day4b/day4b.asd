;;;; day4b.asd

(asdf:defsystem #:day4b
  :description "Describe day4b here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "day4b")))

