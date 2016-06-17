;;;; git-pack.lisp
(defpackage #:gitplot.git-pack
  (:use #:cl #:cl-annot.class #:alexandria #:gitplot.utils)
  (:export main))

(in-package #:gitplot.git-pack)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Pack file format
;;----------------------------------------------------------------------------
(defun parse-pack-file (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((header (make-array 4
                              :element-type '(unsigned-byte 8)
                              :fill-pointer t)))
      (read-sequence header stream :end 4)
      ;; check header word
      (when (and (string= (babel:octets-to-string header) "PACK")
                 ;; and version = 2
                 (= 2 (nibbles:read-ub32/be stream)))
        (let ((objects-count (nibbles:read-ub32/be stream)))
          (values objects-count (read-pack-entry stream)))))))


(defun read-pack-entry (stream)
  (read-pack-entry-header stream))

(defun read-pack-entry-header (stream)
  "Reads the current stream for git pack entry header.
Pack entry header format (from the official documentation
on https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt
``
1-byte size extension bit (MSB)
       type (next 3 bit)
       size0 (lower 4-bit)
n-byte sizeN (as long as MSB is set, each 7-bit)
        size0..sizeN form 4+7+7+..+7 bit integer, size0
	is the least significant part, and sizeN is the
	most significant part.
``
Example: encoded entry of type 1 and of size 7877.
Binary representation: 1 1110 1100 0101
Let's group it  by 7 7 4 bits
1111011000101
aabbbbbbbcccc

First group(cccc) is 0101 = 5. Let's add type and msb.
Since the type is 1 then the first byte is 149:
149 = 1001 0101
      |^^^ ~~~~                                                            
 MSB--+ |   |
 type---+   |
 value------+

second byte is 110 1100. Add msb = 1110 1100 = 236
Third 11 (the last byte, msb not needed) = 0000 0011 = 3
so it gives us 3 bytes : 149 236 3.

Decoding.
Take first byte: 149
149 apply masks (remove msb and type: (x & 112)>>4) -> 5
Second byte: 236
Remove msb -> 108, shift << 4 = 1728.
1728 + 5(extracted from 1st byte) = 1733
Third byte, no msb -> 3, shift 11 (4 + 7) = 6144
And finally the length is 6144 + 1733 = 7833
The function returns (type . len) cons.
"
  (declare (optimize (float 0)))
  (let* ((head (read-byte stream))
         (shift 4) ; first part of size is shifted by 4 bits
         (type (ash (logand head 112) (- shift)))
         (len (logand 15 head)))
    (loop while (>= head 128)
          do
          (progn 
            (setf head (read-byte stream))
            (incf len (ash (logand 127 head) shift))
            (incf shift 7)))
    (cons type len)))
                              
