;;;; git-api.lisp
(defpackage #:gitplot.git-api
  (:use #:cl #:cl-annot.class #:alexandria
   #:gitplot.utils #:gitplot.git-pack #:gitplot.git-object))

(in-package #:gitplot.git-api)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Repository class
;;----------------------------------------------------------------------------
@export-class
(defclass git-repo ()
  ((path :initarg :path :reader repo-path)
   (pack-files :reader pack-files :initform nil))
  (:documentation "Class representing git repository"))


@export
(defun make-git-repo (path)
  (make-instance 'git-repo :path path))


(defmethod initialize-instance :after ((self git-repo) &key &allow-other-keys)
  "Constructor for the git-repo class"
  (with-slots (path pack-files) self
    ;; append trailing "/"
    (unless (ends-with "/" path)
      (setf path (concatenate 'string path "/")))
    (let ((files (directory (concatenate 'string path ".git/objects/pack/*.pack"))))
      (mapcar (lambda (pack) (push (parse-pack-file (namestring pack)) pack-files)) files))))


@export
(defmethod get-object-by-hash ((self git-repo) hash)
  "Returns the object by the given hash string"
  ;; first try if the file exists
  (with-slots (path pack-files) self
    (let ((file-object-name (concatenate 'string
                                         path
                                         ".git/objects/"
                                         (subseq hash 0 2)
                                         "/"
                                         (subseq hash 2))))
      ;; probe if exists
      (if (fad:file-exists-p file-object-name)
          (parse-git-file file-object-name)
          ;; no file exist
          (let* ((result nil)
                 (found-pack
                  ;; iterate oven all pack files trying to find the one having hash
                  (loop for pack in pack-files
                        when (setf result (gethash hash (index-table pack))) return pack)))
            ;; ok pack and corresponding index entry found 
            (when (and result found-pack)
              ;; get the data from pack file
              (let ((data (pack-get-object-by-hash found-pack hash)))
                (when data
                  ;; and finally parse the data (the type is known from index)
                  (parse-git-object (pack-entry-type result)
                                    data
                                    :start 0
                                    :size (length data))))))))))
                
      
      




