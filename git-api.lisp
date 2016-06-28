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
    (let ((files (directory (concatenate 'string path ".git/objects/pack/*.idx"))))
      (mapcar (lambda (pack) (push (gitplot.git-pack::parse-index-file (namestring pack)) pack-files)) files))))


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
                                    hash
                                    :start 0
                                    :size (length data))))))))))
                
@export
(defmethod get-head-hash ((self git-repo))
  (with-slots (path) self
    (let* ((head-file (concatenate 'string path ".git/HEAD"))
           (head-contents (read-one-line head-file)))
      ;; check if the HEAD points to the detached commit
      (if (not (starts-with-subseq "ref: " head-contents))
          head-contents
          ;; otherwise search if this file exists
          (let* ((head-ref (cadr (split-sequence:split-sequence #\space head-contents)))
                 (head-ref-file (concatenate 'string path ".git/" head-ref)))
            ;; check if head points to ref which is a normal file
            (if (fad:file-exists-p head-ref-file)
                (read-one-line head-ref-file)
                ;; otherwise read the ref from the packed-refs
                (with-open-file (stream (concatenate 'string path ".git/packed-refs")
                                        :direction :input)
                  (loop for line = (read-line stream nil)
                        while (and line (not (ends-with-subseq head-ref line)))
                        finally (car (split-sequence:split-sequence #\space line))))))))))


@export
(defmethod get-head-commit ((self git-repo))
  (get-object-by-hash self (get-head-hash self)))


@export
(defmethod get-commit-parents ((self git-repo) (object gitplot.git-object:commit))
  (mapcar (curry #'get-object-by-hash self) (commit-parents object)))


