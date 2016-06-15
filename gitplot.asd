;;;; gitplot.asd

(asdf:defsystem #:gitplot
  :description "Plot the branches tree of the git repository"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "Unknown yet"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-annot
               #:babel
               #:zlib
               #:split-sequence
               #:ironclad)
  :serial t
  :components ((:file "utils")
               (:file "git-api")
               (:file "app")))

