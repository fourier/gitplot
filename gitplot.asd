;;;; gitplot.asd

(asdf:defsystem #:gitplot
  :description "Plot the branches tree of the git repository"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "Unknown yet"
  :depends-on (#:alexandria
               #:cl-fad
               #:ironclad)
  :serial t
  :components ((:file "app")))

