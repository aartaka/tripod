;;;; tripod.asd

(asdf:defsystem #:tripod
  :description "Tripod is a Common Lisp web server aiming to ease plain text website hosting."
  :author "Artyom Bologov"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "tripod"
  :entry-point "tripod:entry-point"
  :depends-on (#:alexandria #:trivial-mimes #:local-time
               #:hunchentoot
               #:cl-gopher #:phos #:plump #:clss #:cl-markdown
               #:nactivitypub #:njson)
  :components ((:file "src/package")
               (:file "src/model")
               (:file "src/parse")
               (:file "src/gopher")
               (:file "src/gemini")
               (:file "src/html")
               (:file "src/activitypub")
               (:file "src/cli")))
