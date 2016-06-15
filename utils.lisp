;;;; git-api.lisp
(defpackage #:gitplot.utils
  (:use #:cl #:cl-annot.class #:alexandria)
  (:export main))

(in-package #:gitplot.utils)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Utility functions
;;----------------------------------------------------------------------------
@export
(defun file-size (filename)
  "Return the size of the file with the name FILENAME in bytes"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-length in)))


@export
(defun read-binary-file (filename)
  "Return an array of file contents"
  (let* ((size (file-size filename))
         (buffer (make-array size
                             :element-type '(unsigned-byte 8)
                             :fill-pointer t)))
    (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
      (read-sequence buffer stream))
    buffer))


@export
(defun read-header (filename size)
  "Read SIZE bytes from the file FILENAME. If the file size is less than SIZE,
read up to the size of file"
  (let ((elt-type '(unsigned-byte 8)))
    (with-open-file (in filename :element-type elt-type)
      (let* ((fsize (file-length in))
             (buffer (make-array (min size fsize) :element-type elt-type)))
        (read-sequence buffer in)
        buffer))))


@export
(defun adjacent-find (vec &key (first 0) (last (length vec)) (test #'equal))
  "Find 2 adjacent equal elements in VEC.
Alternatively if TEST given, 2 adjactent elements ONE TWO for those
(TEST ONE TWO) returns not nil.
Returns the index of the first element found or size of VEC if
nothing found"
  (if (/= first last)
      (let ((next (1+ first)))
        (loop while (/= next last)
              do
              (if (funcall test (elt vec first) (elt vec next))
                  (return first)
                  (progn
                    (incf first)
                    (incf next)))
              finally (return last)))
      last))
            

