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
(defclass git-object ()
  ((hash :initarg :hash :reader object-hash :type '(vector unsigned-byte 8)))
  (:documentation "Base class for Git objects"))


(defclass commit (git-object)
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

(defclass blob (git-object)
  ((content :initarg :content :reader blob-content :initform nil))
  (:documentation "Git Blob object"))

(defmethod print-object ((self blob) stream)
  (with-slots (content) self
    (format stream "blob of size ~d bytes~%" (length content))))



(defclass tree (git-object)
  ()
  (:documentation "Git Tree object"))


(defclass tag (git-object)
  ((object :initarg :tree :reader tag-object :initform "")
   (type :initarg :author :reader tag-type :initform "")
   (tagger :initarg :committer :reader tag-tagger :initform "")
   (comment :initarg :comment :reader tag-comment :initform "")
   (tag :initarg :parents :reader tag :initform nil))
  (:documentation "Git Tag object"))

(defmethod print-object ((self tag) stream)
  (with-slots (object type tagger tag comment) self
    (format stream "object ~a~%" object)
    (format stream "type ~a~%" type)
    (format stream "tagger ~a~%" tagger)
    (format stream "tag ~a~%" tag)
    (format stream "comment~%~a" comment))) 


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
                      :size (parse-integer (cadr header-split)))))

(defgeneric parse-git-object (type data &key start size))

(defmethod parse-git-object ((obj (eql 'blob)) data &key start size)
  (let ((blob (make-instance 'blob :content (subseq data start (+ start size)))))
    blob))


(defmethod parse-git-object ((obj (eql 'tree)) data &key start size)
  "tree!")

(defun parse-text-git-data (data start size)
  "Parses the data for text git objects (commit,tag)
and returns a PAIR:
  (car PAIR) = list of lines before the comment
  (cdr PAIR) = comment"
  (let* ((text (babel:octets-to-string data
                                       :start start
                                       :end (+ start size)
                                       :encoding :utf-8))
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
    (cons (split-sequence:split-sequence #\newline header)
          comment)))


(defmethod parse-git-object ((obj (eql 'commit)) data &key start size)
  (let* ((parsed-data (parse-text-git-data data start size))
         (commit (make-instance 'commit :comment (cdr parsed-data))))
    (with-slots (tree author committer parents) commit
      (mapcar
       (lambda (line)
         (let* ((space-pos (position #\Space line))
                (key (subseq line 0 space-pos))
                (value (subseq line (1+ space-pos))))
           (switch (key :test #'string=)
             ("tree" (setf tree value))
             ("author" (setf author value))
             ("committer" (setf committer value))
             ("parent" (push value parents)))))
       (car parsed-data)))
     commit))
    


(defmethod parse-git-object ((obj (eql 'tag)) data &key start size)
  (let* ((parsed-data (parse-text-git-data data start size))
         (self (make-instance 'tag :comment (cdr parsed-data))))
    (mapcar
     (lambda (line)
       (let* ((space-pos (position #\Space line))
              (key (subseq line 0 space-pos))
              (value (subseq line (1+ space-pos))))
         (setf (slot-value self (intern (string-upcase key))) value)))
     (car parsed-data))
    self))


