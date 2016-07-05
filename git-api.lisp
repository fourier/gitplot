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
  ((path :initarg :path :reader repo-path
         :documentation "Path to the repository")
   (pack-files :reader pack-files :initform nil
               :documentation "List of pack-file objects")
   (packed-refs :reader packed-refs :initform (make-hash-table :test #'string=)
                :documentation "Map between ref string and SHA1 text code in packed-refs file")
   (annotated-tags :reader annotated-tags :initform (make-hash-table :test #'string=)
                   :documentation "Map between ref string of annotated tag and the SHA1 text code of the commit this annotated tag points to"))
  (:documentation "Class representing git repository"))


@export
(defun make-git-repo (path)
  (make-instance 'git-repo :path path))


(defmethod initialize-instance :after ((self git-repo) &key &allow-other-keys)
  "Constructor for the git-repo class"
  (with-slots (path pack-files packed-refs annotated-tags) self
    ;; append trailing "/"
    (unless (ends-with "/" path)
      (setf path (concatenate 'string path "/")))
    ;; collect all pack files
    (let ((files (directory (concatenate 'string path ".git/objects/pack/*.pack"))))
      (mapcar (lambda (pack) (push (parse-pack-file (namestring pack)) pack-files)) files))
    ;; read all refs from the packed-refs
    (let ((packed-refs-filename (concatenate 'string path ".git/packed-refs")))
      (when (fad:file-exists-p packed-refs-filename)
        (with-open-file (stream packed-refs-filename
                                :direction :input)
          (let (prev-ref) ; previous ref
            (loop for line = (read-line stream nil)
                  while line
                  ;; skip comments
                  unless (starts-with #\# (string-trim '(#\Space #\Tab) line))            
                  do
                  (let ((ref (split-sequence:split-sequence #\space line)))
                    ;; check if the current line is the peeled ref (points to
                    ;; the line above which is an annotated tag)
                    (if (starts-with #\^ line)
                        (setf (gethash prev-ref annotated-tags)
                              (subseq (car ref) 1))
                        (setf (gethash (cadr ref) packed-refs) (car ref)
                              prev-ref (cadr ref)))))))))))




@export
(defmethod get-object-by-hash ((self git-repo) hash)
  "Returns the object by the given hash string"
  ;; first try if the file exists
  (with-slots (path pack-files) self
    ;; no file exist
    (let* ((result nil)
           (found-pack
            ;; iterate oven all pack files trying to find the one having hash
            (loop for pack in pack-files
                  when
                  (= (length
                      (setf result (multiple-value-list
                                    (pack-get-object-by-hash pack hash))))
                     2)
                  return pack)))
      ;; ok pack and corresponding index entry found 
      (if (and result found-pack)
          ;; get the data from pack file
          ;; pack-get-object-by-hash returns values: (data, type)
          (let ((data (car result))
                (type (cadr result)))
            (when data
              ;; and finally parse the data
              (parse-git-object type
                                data
                                hash
                                :start 0
                                :size (length data))))
          (let ((file-object-name (fad:file-exists-p (concatenate 'string
                                                                  path
                                                                  ".git/objects/"
                                                                  (subseq hash 0 2)
                                                                  "/"
                                                                  (subseq hash 2)))))
            ;; probe if exists
            (when file-object-name
              (parse-git-file file-object-name)))))))


@export
(defmethod get-head-hash ((self git-repo))
  (with-slots (path) self
    (let* ((head-file (concatenate 'string path ".git/HEAD"))
           (head-contents (read-one-line head-file)))
      ;; check if the HEAD points to the detached commit
      (if (not (starts-with-subseq "ref: " head-contents))
          head-contents
          ;; otherwise search if this file exists
          (let* ((head-ref (cadr (split-sequence:split-sequence #\space head-contents))))
            (get-hash-by-ref self head-ref))))))


(defmethod get-hash-by-ref ((self git-repo) ref)
  "Returns the hash string by given ref string.
Examples of ref strings:
ref/heads/master
refs/tags/v1.0"
  (with-slots (packed-refs path) self
    (let ((ref-file (concatenate 'string path ".git/" ref)))
      ;; check if the ref is a normal file
      (if (fad:file-exists-p ref-file)
          (read-one-line ref-file)
          ;; otherwise find in packed refs
          (gethash ref packed-refs)))))
    


@export
(defmethod get-head-commit ((self git-repo))
  (get-object-by-hash self (get-head-hash self)))


@export
(defmethod get-commit-parents ((self git-repo) (object gitplot.git-object:commit))
  (mapcar (curry #'get-object-by-hash self) (commit-parents object)))


(defmethod get-commit-tree ((self git-repo) (object gitplot.git-object:commit))
  (let ((tree nil)
        (children (list object)))
    (loop while children
          do
          (progn
            (let* ((current (pop children))
                   (kids (get-commit-parents self current)))
              (mapcar (lambda (x) (push (object-hash x) tree) (push x children)) kids))))
    tree))