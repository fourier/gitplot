;;;; git-api.lisp
(defpackage #:gitplot.utils
  (:use #:cl #:cl-annot.class #:alexandria)
  (:export main))

(in-package #:gitplot.utils)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Utility macros
;;----------------------------------------------------------------------------
@export
(defmacro from (package import name &rest others)
  "Import symbol(s) NAME ... from the package PACKAGE.
Examples:
(from mediaimport.utils import interleave partition +regex-escape-chars+)
(from mediaimport.ui import save-edit-controls-history)
(from mediaimport.utils import *)
In the last example imports all the exported symbols from the package given."
  (unless (string-equal import 'import)
    (error "Unexpected keyword: expected IMPORT, got ~A" import))
  (let* ((pkg (string-upcase (symbol-name package))) ;; package name as a string
         (symbols ; symbols to be imported
          (if (and (not others) (string-equal name "*"))
              ;; if called like (from something import *)
              (let (symbols)
                (do-external-symbols (s pkg)
                  (push s symbols))
                symbols)
              ;; otherwise just arguments list
              (cons name others))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
       ,@(mapcar (lambda (symb)
                   (let ((import-symbol (find-symbol (string-upcase (symbol-name symb)) pkg)))
                     `(shadowing-import ,(list 'quote import-symbol))))
                 symbols)))))

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
            

@export
(defun sha1-to-hex (input &optional (offset 0))
  "Reads the SHA1 code from either:
- stream,
- vector of unsigned bytes,
- list of integers
returns the downcase string representing SHA1 code in hex format.
NOTE: OFFSET is ignored for streams"
  (typecase input
    ((simple-array (unsigned-byte 8)) (sha1-array-to-hex input offset))
    (list (sha1-list-to-hex input offset))
    (stream (sha1-stream-to-hex input))
    (t nil)))


(defun sha1-array-to-hex (arr offset)
  (string-downcase
   (with-output-to-string (s)  
    (loop for i from offset below (+ offset 20)
          do
          (format s "~2,'0x" (system:octet-ref arr i)))
    s)))

(defun sha1-list-to-hex (lst offset)
  (string-downcase
   (with-output-to-string (s)  
    (loop for i from offset below (+ offset 20)
          do
          (format s "~2,'0x" (nth i lst)))
    s)))


(defun sha1-stream-to-hex (stream)
  (string-downcase
   (with-output-to-string (s)
     (dotimes (x 20)
       (format s "~2,'0x" (read-byte stream)))
    s)))
