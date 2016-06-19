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

;; imports
(from system import octet-ref)
(from nibbles import read-ub32/be ub32ref/be read-ub64/be)
(from babel import octets-to-string)

;;----------------------------------------------------------------------------
;; Pack file format
;;----------------------------------------------------------------------------

(defconstant +pack-file-header-word+ "PACK"
  "First bytes of the pack file")

(defconstant +index-file-header-word+ #(255 116 79 99)
  "First bytes of the index file")

(defconstant +pack-version+ 2
  "Version of the pack file format")

(defconstant +sha1-size+ 20
  "Number of bytes of SHA1 checksum")

;;----------------------------------------------------------------------------
;; Constants from https://github.com/git/git/blob/master/cache.h
;;----------------------------------------------------------------------------

(defconstant OBJ-BAD -1)
(defconstant OBJ-NONE 0)
(defconstant OBJ-COMMIT 1)
(defconstant OBJ-TREE 2)
(defconstant OBJ-BLOB 3)
(defconstant OBJ-TAG 4)
;; 5 for future expansion 
(defconstant OBJ-OFS-DELTA 6)
(defconstant OBJ-REF-DELTA 7)
(defconstant OBJ-ANY 8)
(defconstant OBJ-MAX 9)

(defstruct pack-entry 
  offset data-offset compressed-size uncompressed-size type depth base-hash)

(defmethod print-object ((entry pack-entry) stream)
  ;; type size size-in-packfile offset-in-packfile [depth base-SHA-1]
  (format stream "~a ~a ~a ~a~%"
          (switch ((pack-entry-type entry))
            (OBJ-COMMIT "commit")
            (OBJ-TREE "tree")
            (OBJ-BLOB "blob")
            (OBJ-TAG "tag"))
          (pack-entry-uncompressed-size entry)
          (pack-entry-compressed-size entry)
          (pack-entry-offset entry)))


(defun pack-filename-to-index (filename)
  "Convert pack file name to index file name (by replacing extension)"
  (multiple-value-bind (pos len)
      (lw:find-regexp-in-string ".pack$" filename :case-sensitive nil)
    (assert (= len 5))
    (concatenate 'string (subseq filename 0 pos) ".idx")))


(defun index-filename-to-pack (filename)
  "Convert index file name to pack file name (by replacing extension)"
  (multiple-value-bind (pos len)
      (lw:find-regexp-in-string ".idx$" filename :case-sensitive nil)
    (assert (= len 4))
    (concatenate 'string (subseq filename 0 pos) ".pack")))


(defun parse-pack-file (filename)
  ;; first parse index file
  (let ((index (parse-index-file (pack-filename-to-index filename))))
    ;; then open the pack file itself.
    ;; at this point we know offsets of entries in pack file from the index file
    (with-open-file (stream filename
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (let ((header (make-array 4
                                :element-type '(unsigned-byte 8)
                                :fill-pointer t)))
        (read-sequence header stream :end 4)
        ;; check header word
        (when (and (string= (octets-to-string header) +pack-file-header-word+)
                   ;; and version = 2
                   (= +pack-version+ (read-ub32/be stream)))
          (let ((objects-count (read-ub32/be stream)))
            ;; sanity check
            (assert (= objects-count (length index)))
            (create-pack-entries-table index stream)))))))


(defun create-pack-entries-table (index stream)
  (let ((table (make-hash-table :test #'string= :size (length index)))
        (file-length (file-length stream)))
    (loop for i from 0 below (length index) do
          ;; fill the table.
          (progn
            (let ((current-entry (make-pack-entry :offset (cdr (aref index i)))))
              ;; move position to the current entry offset
              (file-position stream (pack-entry-offset current-entry))
              ;; read the header - type (car header)
              ;; and uncompressed size (cdr header)
              (multiple-value-bind (type uncompr-len base-hash base-offset)
                  (read-pack-entry-header stream)
                (setf (pack-entry-type current-entry) type
                      (pack-entry-uncompressed-size current-entry) uncompr-len
                      ;; calculate the compressed size (size in pack file) ...
                      (pack-entry-compressed-size current-entry)
                      ;; ... as difference between offset of the current
                      ;; and next entry ...
                      (- (if (< i (1- (length index)))
                             (cdr (aref index (1+ i)))
                             ;; or end of file(without SHA-1 trailer of 20 bytes)
                             (- file-length 20))
                         (pack-entry-offset current-entry))
                      (pack-entry-data-offset current-entry) (file-position stream))
                ;; handle entries with deltas
                (when (or (= type OBJ-REF-DELTA)
                          (= type OBJ-OFS-DELTA))
                  ;; set the parent hash                  
                  (setf (pack-entry-base-hash current-entry)
                        (switch (type)
                          (OBJ-REF-DELTA base-hash)
                          (OBJ-OFS-DELTA (car (find (- (pack-entry-offset current-entry)
                                                       base-offset)
                                                    index :key #'cdr)))))))                        
              ;; missing: depth
              (setf (gethash (car (aref index i)) table) current-entry))))
    ;; update all types for delta entries, taking from most base entry
    (maphash (lambda (hash entry)
               (declare (ignore hash))
               (when (or (= (pack-entry-type entry) OBJ-REF-DELTA)
                         (= (pack-entry-type entry) OBJ-OFS-DELTA))
                 (multiple-value-bind (type depth)
                     (get-base-pack-entry-type table entry)
                   (setf (pack-entry-type entry) type
                         (pack-entry-depth entry) depth))))
             table)
    table))


(defun get-base-pack-entry-type (table entry)
  "Returs VALUES(type, depth) for the given HASH in hash-table TABLE"
  (let ((parent-hash nil)
        (type nil)
        (parent entry)
        (depth -1))
    (loop do
          (setf type (pack-entry-type parent)
                parent-hash (pack-entry-base-hash parent)
                parent (gethash parent-hash table)
                depth (1+ depth))
          while parent)
    (values type depth)))


(defun read-pack-entry-header (stream)
  "Reads the current stream for git pack entry header.
Returns the following VALUES list (use multiple-value-bind etc.):
(TYPE LEN BASE-HASH BASE-OFFSET)
where:
TYPE is type field of values OBJ-.. (see constants above)
LEN is the length of the uncompressed data
BASE-HASH if the type is OBJ-REF-DELTA the hash value of the base
object for the current object (which is delta). Base hash is a hex string.
BASE-OFFSET if the type is OBJ-OFS-DELTA the relative offset in the file
to the base object.

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
      '^^^ ~~~~                                                            
      | |   |
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
"
  (declare (optimize (float 0)))
  (let* ((base-hash nil)
         (base-offset nil)
         (head (read-byte stream))
         (shift 4) ; first part of size is shifted by 4 bits
         ;; type is encoded first (after MSB)
         (type (ash (logand head 112) (- shift)))
         (len (logand 15 head)))
    ;; calculate variable-length integer (uncompressed size)
    (loop while (>= head 128)
          do
          (progn 
            (setf head (read-byte stream))
            (incf len (ash (logand 127 head) shift))
            (incf shift 7)))
    ;; check if type is OBJ-REF-DELTA or OBJ-OFS-DELTA
    (switch (type)
      (OBJ-REF-DELTA
       ;; just read the base hash
       (setf base-hash (sha1-to-hex stream)))
      (OBJ-OFS-DELTA
       ;; read the variable-length integer
       (setf base-offset (read-network-vli stream))))
    (values type len base-hash base-offset)))

(defun read-network-vli (stream)
  "Read the variable-length integer from the stream"
  (declare (optimize (float 0)))
  (let* ((head (read-byte stream))
         (value (logand 127 head)))
    (loop while (>= head 128)
          do
          (progn
            (incf value)
            (setf head (read-byte stream))
            (setf value (+ (ash value 7) (logand head 127)))))
    value))
  
                              
;;----------------------------------------------------------------------------
;; Index file format
;;----------------------------------------------------------------------------
(defun parse-index-file (filename)
  "Parse the pack index file. Returns an array of pairs: SHA1 hex string and offset"
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((header (make-array 4
                              :element-type '(unsigned-byte 8)
                              :fill-pointer t)))
      ;; 1. read the header 4 bytes
      (read-sequence header stream :end 4)
      ;; check header word
      (when (and (equalp header +index-file-header-word+)
                 ;; read the version 4 bytes and check version = 2
                 (= +pack-version+ (read-ub32/be stream)))
        ;; 2. read the fanout table
        (let* ((fanout (read-fanout-table stream))
               (size (aref fanout 255))
               ;; 3. read sorted list of object names
               (object-names (read-object-names stream size))
               ;; 4. read CRCs of compressed objects
               (crcs (read-crcs stream size))
               ;; 5. and finally read offsets in the PACK file
               (offsets (read-offsets stream size))
               ;; an array of CONSes - object SHA1 code and its offset
               (pack-index-entries (make-array size)))
          (declare (ignore crcs))
          (loop for i from 0 below size do
                (setf (aref pack-index-entries i)
                      (cons (sha1-to-hex (aref object-names i)) (aref offsets i))))
          ;; sort pack index entries according to offset
          (sort pack-index-entries #'< :key #'cdr))))))
               
          

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
          (setf (aref fanout i) (read-ub32/be stream)))
    fanout))


(defun read-object-names (stream size)
  "Read the SIZE 20-bytes SHA1 codes of objects stored in PACK file"
  (let ((object-names (make-array size :element-type '(vector unsigned-byte 8))))
    (loop for i from 0 below size do
          (let ((hash (make-array +sha1-size+ :element-type '(unsigned-byte 8))))
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
          (if (>= (octet-ref table i) 128)
              ;; if set we clear the msb 
              (progn
                (setf (octet-ref table i) (logand 127 (octet-ref table i)))
                ;; and push to the list of "big offsets" as a pair:
                ;; index in original table and index in big offsets table
                (push (cons (ash i -2) (ub32ref/be table i)) big-offsets))
              ;; otherwise just NTOHL the value into the offsets array
              (setf (aref offsets (ash i -2)) (ub32ref/be table i))))
    ;; WARNING!
    ;; the code below for large (> 2gb) files in pack files
    ;; has not been tested!!!
    ;; after processing of small offsets let's process big offsets
    (when big-offsets
      ;; read all values from the stream
      (let ((big-offsets-table
             (make-array (length big-offsets) :element-type 'integer)))
        (dotimes (i (length big-offsets))
          (setf (aref big-offsets-table i) (read-ub64/be stream)))
        ;; process the list of needed big offsets and update original
        ;; offsets array
        (dolist (x big-offsets)
          (setf (aref offsets (car x))
                (aref big-offsets-table (cdr x))))))
    offsets))


