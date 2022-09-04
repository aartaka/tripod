;;;; tripod.asd

(asdf:defsystem #:tripod
  :description "Tripod is a Common Lisp web server aiming to ease plain text website hosting."
  :author "Artyom Bologov"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "tripod"
  :entry-point "tripod::entry-point"
  :pathname "src"
  :depends-on (#:alexandria #:trivial-mimes
               #:hunchentoot #:cl-gopher #:phos #:plump #:clss #:cl-markdown)
  :components ((:file "package")
               (:file "model")
               (:file "parse")
               (:file "gopher")
               (:file "gemini")
               (:file "html")
               (:file "tripod")
               (:file "cli")))
