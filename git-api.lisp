;;;; git-api.lisp
(defpackage #:gitplot.git-api
  (:use #:cl #:cl-annot.class #:alexandria #:gitplot.utils)
  (:export main))

(in-package #:gitplot.git-api)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Repository class
;;----------------------------------------------------------------------------

(defclass repository ()
  ((path :initarg :path :reader repo-path))
  (:documentation "Class representing git repository"))


;;----------------------------------------------------------------------------
;; Git Object class
;;----------------------------------------------------------------------------
(defclass object ()
  ((hash :initarg :hash :reader object-hash :type '(vector unsigned-byte 8)))
  (:documentation "Base class for Git objects"))


(defclass commit (object)
  ((tree :initarg :tree :reader commit-tree :initform "")
   (author :initarg :author :reader commit-author :initform "")
   (committer :initarg :committer :reader commit-committer :initform "")
   (comment :initarg :comment :reader commit-comment :initform "")
   (parents :initarg :parents :reader commit-parents :initform nil))
  (:documentation "Git Commit object"))

(defmethod print-object ((self commit) stream)
  (with-slots (tree author committer comment parents) self
    (format stream "tree ~a~%" tree)
    (format stream "author ~a~%" author)
    (format stream "committer ~a~%" committer)
    (format stream "parents ~{~a~^, ~}~%" parents)
    (format stream "comment~%~a" comment))) 

(defclass blob (object)
  ()
  (:documentation "Git Blob object"))


(defclass tree (object)
  ()
  (:documentation "Git Tree object"))


(defclass tag (object)
  ()
  (:documentation "Git Tag object"))

(defconstant blob-tag 'blob)

(defun parse-git-file (filename)
  (let* ((data (zlib:uncompress (read-binary-file filename)))
         (content-start (position 0 data))
         (header 
          (babel:octets-to-string data :start 0 :end content-start
                                  :encoding :utf-8))
         (header-split (split-sequence:split-sequence #\Space header)))
    (parse-git-object (intern (string-upcase (car header-split)))
                      data
                      :start (1+ content-start)
                      :end (parse-integer (cadr header-split)))))

(defgeneric parse-git-object (type data &key start end))

(defmethod parse-git-object ((obj (eql 'blob)) data &key start end)
  "blob!")


(defmethod parse-git-object ((obj (eql 'tree)) data &key start end)
  "tree!")


(defmethod parse-git-object ((obj (eql 'commit)) data &key start end)
  (let* ((text (babel:octets-to-string data :start start :encoding :utf-8))
         (last-char-pos (1- (length text)))
         (newline-position (min (adjacent-find text
                                               :first start
                                               :last (length text)
                                               :test (lambda (x y)
                                                       (and (char= x y)
                                                            (char= x #\newline))))
                                last-char-pos))
         (header (subseq text 0 newline-position))
         (comment (if (> last-char-pos newline-position)
                      (subseq text (+ 2 newline-position) last-char-pos)
                      "")))
    (make-instance 'commit :comment comment)))
    
    


(defmethod parse-git-object ((obj (eql 'tag)) data &key start end)
  "tag!")



