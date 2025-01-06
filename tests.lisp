(defpackage :binary-io/test
  (:use :common-lisp :binary-io :binary-io.common-datatypes :1am))

(in-package :binary-io/test)

(defun prepare-file (name)
  (with-open-file (fd name :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (write-value :u16 fd #xdead)
    (write-value :vector fd (vector 1078530011 1078530011) :type :u32 :size 2)
    (write-value :u64 fd 4614256656552045848)))

(test endianness
  (let ((name (format nil "/tmp/~x" (random #xffffff)))
        (*endianness* :little))
    (prepare-file name)
    (with-open-file (fd name :element-type '(unsigned-byte 8))
      (is (= (read-value :u16 fd) #xdead))
      (file-position fd 0)
      (let ((*endianness* :big))
        (is (= (read-value :u16 fd) #xadde)))
      (let ((pi32 (float pi 1f0)))
        (is (equalp (read-value :vector fd :type :float32 :size 2)
                    (vector pi32 pi32))))
      (is (= (read-value :float64 fd) pi))
      (file-position fd 2)
      (let ((*endianness* :big))
        (is (= (read-value :u32 fd) #xdb0f4940))))
    (delete-file name)))

(defun prepare-two (name)
  (with-open-file (fd name :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (write-value :u32 fd #xfffefdfc)
    (write-value :vector fd (vector 65437 65437 65437) :type :u16 :size 3)))

(test two-complement
  (let ((name (format nil "/tmp/~x" (random #xffffff)))
        (*endianness* :little))
    (prepare-two name)
    (with-open-file (fd name :element-type '(unsigned-byte 8))
      (is (= (read-value :s8 fd) -4))
      (is (= (read-value :s8 fd) -3))
      (is (= (read-value :s8 fd) -2))
      (is (= (read-value :s8 fd) -1))
      (is (equalp (read-value :vector fd :type :s16 :size 3)
                  (vector -99 -99 -99))))
    (delete-file name)))

(test pack/unpack
  (let ((*endianness* :little)
        (v1 (vector 2 3 4 6 8 9 10 12))
        (v2 (vector 1976 1977 2002 2011)))
    (is (equalp v1 (unpack (pack v1 2) 2)))
    (is (equalp v1 (unpack (pack v1 4) 4)))
    (is (equalp v1 (unpack (pack v1 8) 8)))
    (is (equalp v2 (pack (unpack v2 2) 2)))
    (is (equalp (vector 3 0 3) (pack #*110011 2 1)))
    (is (loop for i across (unpack (vector #xff) 8 1) always (= i 1)))))

(defun prepare-null-string (name)
  (with-open-file (fd name :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (format fd "Hello😀~c" #\Nul)))

(defun read-back-with-type (name type)
  (with-open-file (fd name :element-type '(unsigned-byte 8))
    (read-value type fd)))

(test null-string
  (let ((name (format nil "/tmp/~x" (random #xffffff))))
    (prepare-null-string name)
    ;; Define fixed and variable length strings type
    (define-binary-type fixed ()
      (binary-io.common-datatypes:8bit-string :length 10 :terminator #\Nul))
    (define-binary-type variable ()
      (binary-io.common-datatypes:8bit-string :terminator #\Nul))
    (let ((valid (format nil "Hello~c~c~c~c"
                         ;; Decoded 😀
                         (code-char #xF0)
                         (code-char #x9F)
                         (code-char #x98)
                         (code-char #x80))))
      (is (string= (read-back-with-type name 'fixed) valid))
      (is (string= (read-back-with-type name 'variable) valid)))
    ;; Write a fixed and read a variable back
    (with-open-file (fd name :direction :io
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
      (write-value 'fixed fd "Test")
      (file-position fd 0)
      (is (string= (read-value 'variable fd) "Test")))
    ;; Write a variable and read a variable back
    (with-open-file (fd name :direction :io
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
      (write-value 'variable fd "Variable test")
      (file-position fd 0)
      (is (string= (read-value 'variable fd) "Variable test")))
    (delete-file name)))
