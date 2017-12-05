;;;; day4a.asd

(asdf:defsystem #:day4a
  :description "Describe day4a here"
  :author "sergio.garcia@gmail.com"
  :license "artistic license"
  :depends-on (#:alexandria #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "day4a")))

