;;;; git-pack.lisp
;;
;; This package reads the git pack and index files
;; according to the official documentation:
;; https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt
;;
;; See also this blog entry:
;; https://codewords.recurse.com/issues/three/unpacking-git-packfiles/
;;
;; To understand how the pack file is constructed also helps to
;; read the source: https://github.com/git/git/blob/master/pack-write.c
;;
(defpackage #:gitplot.git-pack
  (:use #:cl #:cl-annot.class #:alexandria #:gitplot.utils)
  (:export main))

(in-package #:gitplot.git-pack)
(annot:enable-annot-syntax)

;; 

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
on https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt)
-------------------------------------------------------
1-byte size extension bit (MSB)
       type (next 3 bit)
       size0 (lower 4-bit)
n-byte sizeN (as long as MSB is set, each 7-bit)
        size0..sizeN form 4+7+7+..+7 bit integer, size0
	is the least significant part, and sizeN is the
	most significant part.
-------------------------------------------------------
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
The function returns (TYPE . LEN) cons.
LEN here is the the size of unpacked data.
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
                              
;;----------------------------------------------------------------------------
;; Index file format
;;----------------------------------------------------------------------------
(defun parse-index-file (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((header (make-array 4
                              :element-type '(unsigned-byte 8)
                              :fill-pointer t)))
      ;; 1. read the header 4 bytes
      (read-sequence header stream :end 4)
      ;; check header word
      (when (and (equalp header #(255 116 79 99))
                 ;; read the version 4 bytes and check version = 2
                 (= 2 (nibbles:read-ub32/be stream)))
        ;; 2. read the fanout table
        (let* ((fanout (read-fanout-table stream))
               (size (aref fanout 255))
               ;; 3. read sorted list of object names
               (object-names (read-object-names stream size))
               (crcs (read-crcs stream size))
               (offsets (read-offsets stream size)))
          offsets)))))
          

(defun read-fanout-table (stream)
  "Reads the fanout table for index file from stream.
According to the documentation (https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt):
----------------------------------------------------------
256 4-byte network byte order
integers.  N-th entry of this table records the number of
objects in the corresponding pack, the first byte of whose
object name is less than or equal to N.  
----------------------------------------------------------
One can deduct number of entries in pack file - it should be in the last
element of the table (since any byte sequence starts with the number
less or equal to 256, as the byte <= 256"
  (let ((fanout (make-array 256 :element-type 'integer :fill-pointer t)))
    (loop for i from 0 to 255
          do
          (setf (aref fanout i) (nibbles:read-ub32/be stream)))
    fanout))


(defun read-object-names (stream size)
  "Read the SIZE 20-bytes SHA1 codes of objects stored in PACK file"
  (let ((object-names (make-array size :element-type '(vector unsigned-byte 8))))
    (loop for i from 0 below size do
          (let ((hash (make-array 20 :element-type '(unsigned-byte 8))))
            (read-sequence hash stream)
            (setf (aref object-names i) hash)))
    object-names))


(defun read-crcs (stream size)
  "Read SIZE CRCs codes from the stream.
Retuns an array of size SIZE with elements 4-bytes arrays with CRC codes"
  (let* ((crcs (make-array size :element-type '(vector unsigned-byte 8))))
    (loop for i from 0 below size do
          (let ((crc (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence crc stream)
            (setf (aref crcs i) crc)))
    crcs))
          
    
(defun read-offsets (stream size)
  "Read offset table[s] and return the array of offsets in PACK file"
  (let* ((offsets (make-array size :element-type 'integer))
         (big-offsets nil)
         (bytes-size (* size 4))
         (table (make-array bytes-size :element-type '(unsigned-byte 8))))
    ;; we will read all the table into bytes array
    (read-sequence table stream)
    (loop for i from 0 below bytes-size by 4 do
          ;; processing separately depending if the MSB is set on the first
          ;; byte of encoded length
          (if (>= (aref table i) 128)
              ;; if set we clear the msb 
              (progn
                (setf (aref table i) (logand 127 (aref table i)))
                ;; and push to the list of "big offsets" as a pair:
                ;; index in original table and index in big offsets table
                (push (cons (ash i -2) (nibbles:ub32ref/be table i)) big-offsets))
              ;; otherwise just NTOHL the value into the offsets array
              (setf (aref offsets (ash i -2)) (nibbles:ub32ref/be table i))))
    ;; WARNING!
    ;; the code below for large (> 2gb) files in pack files
    ;; has not been tested!!!
    ;; after processing of small offsets let's process big offsets
    (when big-offsets
      ;; read all values from the stream
      (let ((big-offsets-table
             (make-array (length big-offsets) :element-type 'integer)))
        (dotimes (i (length big-offsets))
          (setf (aref big-offsets-table i) (nibbles:read-ub64/be stream)))
        ;; process the list of needed big offsets and update original
        ;; offsets array
        (dolist (x big-offsets)
          (setf (aref offsets (car x))
                (aref big-offsets-table (cdr x))))))
    offsets))
