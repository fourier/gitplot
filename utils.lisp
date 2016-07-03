;;;; git-api.lisp
(defpackage #:gitplot.utils
  (:use #:cl #:cl-annot.class #:alexandria)
  (:export main))

(in-package #:gitplot.utils)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(defconstant *zero-ascii-begin* (char-code #\0))
(defconstant *char-ascii-begin* (char-code #\a))


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


@export
(defmacro read-one-line (filename)
  "Read exactly one first line from the file"
  (let ((stream-var (gensym)))
    `(with-open-file (,stream-var ,filename :direction :input)
       (read-line ,stream-var))))


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


(defun sha1-list-to-hex (lst offset)
  (string-downcase
   (with-output-to-string (s)  
    (loop for i from offset below (+ offset 20)
          do
          (format s "~2,'0x" (nth i lst)))
    s)))


(defun sha1-array-to-hex (array offset)
  (declare (optimize (speed 3) (safety 0) (float 0)))
  (declare ((unsigned-byte 8) num))
  (macrolet ((digit-to-hex (dig)
               (let ((digit-var (gensym)))
                 `(let ((,digit-var ,dig))
                    (if (< ,dig 10)
                        (+ *zero-ascii-begin* ,dig)
                        (+ (- ,dig 10) *char-ascii-begin*))))))
    (let ((hex (make-array 40 :element-type 'character :fill-pointer 0 :adjustable nil))) 
      (dotimes (x 20)
        (declare (integer x))
        (let ((byte (aref array (+ x offset))))
          (declare ((unsigned-byte 8) byte)) 
          (let* ((upper-byte (ash byte -4))
                 (lower-byte (- byte (ash upper-byte 4))))
            (vector-push (code-char (digit-to-hex upper-byte)) hex)
            (vector-push (code-char (digit-to-hex lower-byte)) hex))))
      hex)))

(defun sha1-stream-to-hex (stream)
  (declare (optimize (speed 3) (safety 0)))
  (declare ((unsigned-byte 8) num))
  (macrolet ((digit-to-hex (dig)
               (let ((digit-var (gensym)))
                 `(let ((,digit-var ,dig))
                    (if (< ,dig 10)
                        (+ *zero-ascii-begin* ,dig)
                        (+ (- ,dig 10) *char-ascii-begin*))))))
    (let ((hex (make-array 40 :element-type 'character :fill-pointer 0 :adjustable nil))) 
      (dotimes (x 20)
        (declare (integer x))
        (let ((byte (read-byte stream)))
          (declare ((unsigned-byte 8) byte)) 
          (let* ((upper-byte (ash byte -4))
                 (lower-byte (- byte (ash upper-byte 4))))
            (vector-push (code-char (digit-to-hex upper-byte)) hex)
            (vector-push (code-char (digit-to-hex lower-byte)) hex))))
      hex)))

@export
(defun sha1-hex-to-array (sha1string &optional result)
  "Convert the given sha1 string in hex (with lower case characters)
to the byte array.
If RESULT array is given - write to this array"
  (declare (optimize (speed 3) (safety 0)))
  (declare ((unsigned-byte 8) upper-val))
  (declare ((unsigned-byte 8) lower-val))
  (unless result
    (setf result (make-array 20 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable nil)))
  (macrolet ((hex-to-number (hex)
               (let ((hex-var (gensym)))
                 `(let ((,hex-var (char-code ,hex)))
                    (if (>= ,hex-var *char-ascii-begin*)
                        (+ 10 (- ,hex-var *char-ascii-begin*))
                        (- ,hex-var *zero-ascii-begin*))))))
    (dotimes (x 20)
      (let ((upper-val (hex-to-number (aref sha1string (* x 2))))
            (lower-val (hex-to-number (aref sha1string (1+ (* x 2))))))
        (setf (aref result x) (+ (ash upper-val 4) lower-val)))))
  result)

  
(defun make-vector-view (vector start end)
  "Returns array displaced to the vector (starting with start, ending on end)"
  (make-array (- end start 1)
              :displaced-to vector
              :displaced-index-offset start))

