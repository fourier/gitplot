;;;; gitplot.asd

(asdf:defsystem #:gitplot
  :description "Plot the branches tree of the git repository"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "Unspecified"
  :depends-on (#:alexandria     ; general utilities
               #:cl-fad         ; files manipulation
               #:cl-annot       ; export annotations
               #:cl-ppcre       ; portable regular expressions
               #:git-api)       ; git operations
  :serial t
  :components ((:file "utils")
               (:file "app")))

