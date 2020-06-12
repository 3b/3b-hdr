#++(ql:quickload '(alexandria parachute 3bgl-hdr/test))
(defpackage #:3b-hdr/test
  (:use :parachute :cl)
  (:local-nicknames (:hdr :3b-hdr)))
(in-package 3b-hdr/test)


(defun test-float (data raw-file)
  (let* ((r (alexandria:read-file-into-byte-vector raw-file))
         (b (make-array (* 4 (length data)) :element-type '(unsigned-byte 8))))
    (loop for i from 0 by 4
          for f across data
          do (setf (nibbles:ieee-single-ref/le b i) f))
    (equalp r b)))

(defun test-uint (data raw-file)
  (let* ((r (alexandria:read-file-into-byte-vector raw-file))
         (b (make-array (* 4 (length data)) :element-type '(unsigned-byte 8))))
    (loop for i from 0 by 4
          for f across data
          do (setf (nibbles:ub32ref/le b i) f))
    (equalp r b)))

(defparameter *test-dir* (asdf:system-relative-pathname '3b-hdr "test/"))

(defmacro test-file/rgb9e5 (file w h raw &optional (exp 1.0))
  `(let* ((*default-pathname-defaults* *test-dir*)
          (hdr (hdr:read-hdr-file ,file :format :rgb9-e5)))
     (is = (hdr:width hdr) ,w)
     (is = (hdr:height hdr) ,h)
     (is = (hdr:exposure hdr) ,exp)
     (is eql (hdr:gl-pixel-format hdr) :rgb)
     (is eql (hdr:gl-pixel-type hdr) :unsigned-int-5-9-9-9-rev)
     (is eql (hdr:gl-internal-format hdr) :rgb9-e5)
     (is eql (hdr:origin hdr) :upper-left)
     (true (test-uint (hdr:data hdr) ,raw))))

(defmacro test-file/float (file w h raw &optional (exp 1.0))
  `(let* ((*default-pathname-defaults* *test-dir*)
          (hdr (hdr:read-hdr-file ,file :format :float)))
     (is = (hdr:width hdr) ,w)
     (is = (hdr:height hdr) ,h)
     (is = (hdr:exposure hdr) ,exp)
     (is eql (hdr:gl-pixel-format hdr) :rgb)
     (is eql (hdr:gl-pixel-type hdr) :float)
     (is eql (hdr:gl-internal-format hdr) :rgb32f)
     (is eql (hdr:origin hdr) :upper-left)
     (true (test-float (hdr:data hdr) ,raw))))

#++
(defun dump (hdr uf ff)
  (let* ((*default-pathname-defaults* *test-dir*)
         (hu (hdr:data (hdr:read-hdr-file hdr)))
         (hf (hdr:data (hdr:read-hdr-file hdr :format :float)))
         (u (make-array (* 4 (length hu)) :element-type '(unsigned-byte 8)))
         (f (make-array (* 4 (length hf)) :element-type '(unsigned-byte 8))))
    (loop for i from 0 by 4
          for fv across hf
          do (setf (nibbles:ieee-single-ref/le f i) fv))
    (loop for i from 0 by 4
          for uv across hu
          do (setf (nibbles:ub32ref/le u i) uv))
    (alexandria:write-byte-vector-into-file u uf)
    (alexandria:write-byte-vector-into-file f ff)))
#++
(progn
  (dump "test1.hdr" "test1.u32" "test1.f32")
  (dump "test2.hdr" "test2.u32" "test2.f32")
  (dump "test3.hdr" "test3.u32" "test3.f32")
  (dump "test4.hdr" "test4.u32" "test4.f32")
  (dump "testclip.hdr" "testclip.u32" "testclip.f32")
  (dump "testclip2.hdr" "testclip2.u32" "testclip2.f32")
  (dump "testclip3.hdr" "testclip3.u32" "testclip3.f32")
  (dump "image1.hdr" "image1.u32" "image1.f32")
  (dump "rgbr4x4.hdr" "rgbr4x4.u32" "rgbr4x4.f32"))

(define-test hdr)
(define-test (hdr rgb9e5)
  (test-file/rgb9e5 "test1.hdr" 7 12 "test1.u32" 2.0)
  (test-file/rgb9e5 "test2.hdr" 19 32 "test2.u32")
  (test-file/rgb9e5 "test3.hdr" 7 12 "test3.u32")
  (test-file/rgb9e5 "test4.hdr" 19 32"test4.u32")
  (test-file/rgb9e5 "testclip.hdr" 19 32 "testclip.u32")
  (test-file/rgb9e5 "testclip2.hdr" 19 32 "testclip2.u32")
  (test-file/rgb9e5 "testclip3.hdr" 19 32 "testclip3.u32")
  (test-file/rgb9e5 "image1.hdr" 128 85 "image1.u32")
  (test-file/rgb9e5 "rgbr4x4.hdr" 4 4 "rgbr4x4.u32"))

(define-test (hdr float)
  (test-file/float "test1.hdr" 7 12 "test1.f32" 2.0)
  (test-file/float "test2.hdr" 19 32 "test2.f32")
  (test-file/float "test3.hdr" 7 12 "test3.f32")
  (test-file/float "test4.hdr" 19 32"test4.f32")
  (test-file/float "testclip.hdr" 19 32 "testclip.f32")
  (test-file/float "testclip2.hdr" 19 32 "testclip2.f32")
  (test-file/float "testclip3.hdr" 19 32 "testclip3.f32")
  (test-file/float "image1.hdr" 128 85 "image1.f32")
  (test-file/float "rgbr4x4.hdr" 4 4 "rgbr4x4.f32"))

;; todo: files wider than 128, writing
#++
(test 'hdr)
