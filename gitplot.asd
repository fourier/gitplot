;;;; gitplot.asd

(asdf:defsystem #:gitplot
  :description "Plot the branches tree of the git repository"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "Unknown yet"
  :depends-on (#:alexandria     ; general utilities
               #:cl-fad         ; files manipulation
               #:cl-annot       ; export annotations
               #:babel          ; bytes to string
               #:zlib           ; zlib to deal with git objects
               #:split-sequence ; general split
               #:nibbles        ; to parse binary data 
               #:ironclad)      ; sha1 checksum
  :serial t
  :components ((:file "utils")
               (:file "git-pack")
               (:file "git-api")
               (:file "app")))

