(in-package #:3b-hdr)
#++ (ql:quickload '(3b-hdr))
;;; file format:
;; see http://radsite.lbl.gov/radiance/refer/filefmts.pdf
;;     http://radsite.lbl.gov/radiance/refer/Notes/picture_format.html
;; text header
;;   `#?RADIANCE` on first line = magic line
;;   `#..` = comment line?
;;   `FOO=...` = key/value line
;;      FORMAT = 32-bit_rle_rgbe or 32-bit_rle_xyze
;;      EXPOSURE = float value to multiply colors by (multiply if more than 1)
;;      COLORCORR = 3x? float values to multiply color comps by
;;      PIXASPECT = pixel aspect ratio (probably ignore)
;;      SOFTWARE, VIEW, PRIMARIES = ignore for now
;;   other non-blank line = command-line of program that processed file?
;;   blank line = end of header
;;   -Y #### +X #### = height, width
;;     (flip sign before X,Y to flip axis, swap x,y to swap x/y axes)
;;   binary image data:
;;     per scan line:
;;       old format:
;;         r,g,b,e 8 bit data (at least 1 of r,g,b > 127, or all 0)
;;         1,1,1,x = old rle, repeat prev color X times
;;       new rle format: (only used if 8< image width < 32767?
;;         2,2,h,l (h<127) scanline length = (+ l (* 256 h))
;;           1 scanline each for r,g,b,e, rle encoded as
;;              run octet: >128 = repeat next octet (n & 127) times
;;                         <128 = copy next N octets
;;              repeat until LENGTH octets, then repeat for next component
;;

;; quick "buffered stream" hack, since using bivalent streams and read-byte
;; is slow
(defclass buf ()
  ((stream :initarg :stream :reader buf-stream)
   (pos :initform 0 :accessor buf-pos)
   (end :initform 0 :accessor buf-end)
   (buf :initform (make-array 8192 :element-type '(unsigned-byte 8)
                                   :initial-element 0)
        :accessor buf-buf)))

(defun buf-empty (buf)
  (>=  (buf-pos buf) (buf-end buf)))

(defun refill-buf (buf)
  (when (buf-empty buf)
    (setf (buf-pos buf) 0
          (buf-end buf) (read-sequence (buf-buf buf) (buf-stream buf)))))

(defun buf-eof (buf)
  (refill-buf buf)
  (buf-empty buf))

(declaim (inline buf-read-byte))
(defun buf-read-byte (buf)
  ;; note: read-scanline ignores this for speed and operates directly
  ;; on the internal buffer
  (if (buf-eof buf)
      (read-byte (buf-stream buf)) ;; trigger an EOF error on original stream
      (aref (buf-buf buf) (1- (incf (buf-pos buf))))))

(defun buf-peek-byte (buf)
  (if (buf-eof buf)
      (read-byte (buf-stream buf)) ;; trigger an EOF error on original stream
      (aref (buf-buf buf) (buf-pos buf))))


(defun buf-read-line (buf)
  (let ((n nil)
        (next nil))
    (prog1
        (babel:octets-to-string
         (coerce (loop until (or
                              (member (setf n (buf-peek-byte buf))
                                      '(10 13))
                              (buf-eof buf))
                       collect (buf-read-byte buf))
                 '(vector (unsigned-byte 8))))
      (unless (buf-eof buf)
        (loop do (buf-read-byte buf)
              while (and (not (buf-eof buf))
                         ;; don't eat consecutive newlines
                         (not (eql n (setf next (buf-peek-byte buf))))
                         (member next
                                 '(10 13))))))))

(defun read-hdr-header (buf)
  (flet ((parse-key (line)
           (when (and line
                      (string/= line "")
                      (char/= #\# (char line 0)))
             (let* ((= (position #\= line))
                    (k (intern (string-upcase
                                (string-trim '(#\space #\newline #\tab)
                                             (subseq line 0 =)))
                               :keyword))
                    (v (when =
                         (string-trim '(#\space #\newline #\tab)
                                      (subseq line (1+ =))))))
               (case k
                 (:format
                  (list k (intern (string-upcase v) :keyword)))
                 (:exposure
                  (list k (parse-number:parse-number v)))
                 (:colorcorr
                  (list k (mapcar 'parse-number:parse-number
                                  ;; should this allow other separators?
                                  (split-sequence:split-sequence
                                   #\space k :remove-empty-subseqs t))))
                 (:pixaspect
                  (list k (parse-number:parse-number v)))
                 (t (if v
                        (list k v)
                        (list :program line)))))))
         (parse-xy (line)
           (destructuring-bind (axis1 dimension1 axis2 dimension2)
               (split-sequence:split-sequence #\space line
                                              :remove-empty-subseqs t)
             (unless (and (string= axis1 "-Y")
                          (string= axis2 "+X"))
               ;; todo: parse out sign/order of X,Y markers, and
               ;; either return origin or transform data to match
               ;; requested origin
               (error "can't handle file with dimensions ~s yet?" line))
             (list :width (parse-integer dimension2)
                   :height (parse-integer dimension1)
                   :origin :upper-left))))
    (loop with exposure = nil
          with colorcorr = nil
          for line = (buf-read-line buf)
          for (k v) = (parse-key (string-trim '(#\space #\newline #\tab) line))
          unless line
            do (error "invalid header parsing .hdr file?")
          until (equal line "")
          if (eq k :exposure)
            do (setf exposure (* v (or exposure 1.0)))
          else if (eq k :colorcorr)
                 do (setf colorcorr (mapcar '* v (or colorcorr '(1.0 1.0 1.0))))
          else if k
                 collect k into kv and collect v into kv
          finally (return (append (parse-xy (buf-read-line buf))
                                  (list :exposure (if (and exposure
                                                           (= exposure 1.0))
                                                      nil
                                                      exposure)
                                        :colorcorr colorcorr)
                                  kv)))))

(defvar *r* (list 0 0))
(defvar *row* nil)

(declaim (inline old-rle-p new-rle-p new-rle-count))
(defun old-rle-p (r g b e)
  (declare (type (unsigned-byte 8) r g b e))
  (when (= r g b 1)
    e))
(defun new-rle-p (r g b e)
  (declare (type (unsigned-byte 8) r g b e))
  (when (and (= r g 2) (< b 127))
    (dpb b (byte 7 8) e)))

(defun new-rle-count (r g b e)
  (declare (type (unsigned-byte 8) r g b e)
           (ignore r g))
  (dpb b (byte 7 8) e))

(defmacro with-line-readers ((%buf) &body body)
  `(let ((buf (buf-buf ,%buf))
         (pos (buf-pos ,%buf))
         (end (buf-end ,%buf)))
     (declare (type (simple-array (unsigned-byte 8) (*)) buf)
              (type (unsigned-byte 32) pos end))
     (labels ((%refill ()
                (setf pos 0
                      end (read-sequence buf (buf-stream ,%buf)))
                (when (zerop end)
                  ;; error on EOF rather than just continuing to
                  ;; return whatever was in buffer even if file ends
                  (read-byte (buf-stream ,%buf))))
              (%read-byte ()
                (when (>= pos end)
                  (%refill)
                  (setf pos 0))
                (aref buf (1- (incf pos))))
              (read-pixel ()
                (values (%read-byte) (%read-byte) (%read-byte) (%read-byte))))
       (declare (inline read-pixel %read-byte)
                (notinline %refill)
                (ignorable #'read-pixel #'%read-byte))
       (unwind-protect
            (progn ,@body)
         (setf (buf-pos %buf) pos
               (buf-end %buf) end)))))

(defmacro ilambda (args &body body)
  (alexandria:with-gensyms (n)
    `(flet ((,n ,args ,@body))
       (declare (inline ,n))
       #',n)))

(defun read-scanline-old/rgb9e5 (%buf r g b e length destination offset)
  (declare (optimize speed)
           (fixnum length offset)
           (type (unsigned-byte 8) r g b e))
  (check-type destination (simple-array (unsigned-byte 32) (*)))
  (with-line-readers (%buf)
    (when (old-rle-p r g b e)
      (error "got old rle pixel as first pixel of scanline?~% ~s ~s ~s ~s"
             r g b e))
    (let ((p 0))
      (declare (fixnum p))
      (labels ((write-pixel (p r g b e)
                 (cond
                   ((<= 0 e 31)
                    (let ((w 0))
                      (setf (ldb (byte 8 1) w) r
                            (ldb (byte 8 10) w) g
                            (ldb (byte 8 19) w) b
                            (ldb (byte 5 27) w) e)
                      (setf (aref destination (+ offset p)) w)))
                   ((minusp e)
                    ;; exponent too small, clamp it and shift mantissa
                    (cond
                      ((< e -9) ;; no bits left, just write 0
                       (setf (aref destination (+ offset p)) 0))
                      (t
                       (let* ((w2 0))
                         (declare (type (unsigned-byte 32) w2))
                         (setf (ldb (byte 9 0) w2) (ash r (1+ e)))
                         (setf (ldb (byte 9 9) w2) (ash g (1+ e)))
                         (setf (ldb (byte 9 18) w2) (ash b (1+ e)))
                         (setf (aref destination (+ offset p)) w2)))))
                   ((> e 31)
                    (decf e 31)
                    (cond
                      ((> e 9) ;; no bits left, write white
                       (setf (aref destination (+ offset p)) #xffffffff))
                      (t ;; scale and saturate components
                       (let* ((w2 0))
                         (declare (type (unsigned-byte 32) w2))
                         (setf (ldb (byte 9 0) w2) (min 511 (ash r (1+ e))))
                         (setf (ldb (byte 9 9) w2) (min 511 (ash g (1+ e))))
                         (setf (ldb (byte 9 18) w2) (min 511 (ash b (1+ e))))
                         (setf (ldb (byte 5 27) w2) #b11111)
                         (setf (aref destination (+ offset p)) w2)))))
                   (t (break "shouldn't happen?") 0))))
        (declare (inline write-pixel))
        (write-pixel p r g b (- e 113))
        (incf p)
        (loop
          with rle of-type (or null (unsigned-byte 16)) = 0
          with lw of-type (unsigned-byte 32) = 0
          while (< p length)
          do (multiple-value-bind (r g b e)
                 (read-pixel)
               (declare (type (unsigned-byte 8) r g b e))
               (cond
                 ((setf rle (old-rle-p r g b e))
                  (break "rle")
                  (let ((start (+ offset p)))
                    (declare (fixnum start))
                    (fill destination lw :start start
                                         :end (+ start rle))
                    (incf p rle)))
                 (t
                  (setf lw (write-pixel p r g b (- e 113)))
                  (incf p)))))))))

(defun read-scanline-old/float (%buf r g b e length destination offset)
  (declare (optimize speed)
           (fixnum length offset)
           (type (unsigned-byte 8) r g b e))
  (check-type destination (simple-array (single-float) (*)))
  (with-line-readers (%buf)
    (when (old-rle-p r g b e)
      (error "got old rle pixel as first pixel of scanline?~% ~s ~s ~s ~s"
             r g b e))
    (let ((p 0)
          (lr 0)
          (lg 0)
          (lb 0)
          (le 0))
      (declare (type (unsigned-byte 16) p))
      (labels ((write-pixel (p r g b e)
                 (declare (type (unsigned-byte 16) p))
                 (let ((p2 (* p 3))
                       (e2 (expt 2.0 (- e (+ 128 8)))))
                   (setf (aref destination (+ offset p2 0))
                         (* r e2))
                   (setf (aref destination (+ offset p2 1))
                         (* g e2))
                   (setf (aref destination (+ offset p2 2))
                         (* b e2)))))
        (declare (inline write-pixel))
        (write-pixel p r g b e)
        (incf p)
        (loop
          with rle of-type (or null (unsigned-byte 16)) = 0
          while (< p length)
          do (multiple-value-bind (r g b e)
                 (read-pixel)
               (declare (type (unsigned-byte 8) r g b e))
               (cond
                 ((setf rle (old-rle-p r g b e))
                  (let* ((e2 (expt 2.0 (- e (+ 128 8))))
                         (r (* r e2))
                         (g (* g e2))
                         (b (* b e2)))
                    (loop for p2 from (+ offset (* 3 p))
                            below (- (length destination) 2) by 3
                          repeat rle
                          do (setf (aref destination (+ p2 0)) r)
                             (setf (aref destination (+ p2 1)) g)
                             (setf (aref destination (+ p2 2)) b)))
                  (incf p rle))
                 (t
                  (write-pixel p r g b e)
                  (setf lr r lg g lb b le e)
                  (incf p)))))))))

(defun read-scanline-old (%buf r g b e length destination offset type)
  (ecase type
    ((:rgb9-e5)
     (read-scanline-old/rgb9e5 %buf r g b e length destination offset))
    (:float (read-scanline-old/float %buf r g b e length destination offset))))
(defvar *c* 0)

(defun read-scanline-new (%buf r g b e length destination offset type)
  (declare (optimize speed)
           (fixnum offset)
           (type (unsigned-byte 16) length)
           (type (unsigned-byte 8) r g b e))
  (check-type destination (or (simple-array (unsigned-byte 32) (*))
                              (simple-array single-float (*))))
  (with-line-readers (%buf)
    (let ((rle 0)
          (rs (make-array length :element-type '(unsigned-byte 8)))
          (gs (make-array length :element-type '(unsigned-byte 8)))
          (bs (make-array length :element-type '(unsigned-byte 8))))
      (declare (type (unsigned-byte 16) rle))
      (labels ((rle-component (f)
                 (let ((p2 0))
                   (declare (type (unsigned-byte 16) p2))
                   (loop
                     until (not (< p2 rle))
                     do (let ((r2 (%read-byte)))
                          (declare (type (unsigned-byte 8) r2))
                          (if (> r2 128)
                              (loop with v = (%read-byte)
                                    repeat (ldb (byte 7 0) r2)
                                    do (funcall f p2 v)
                                       (incf p2))
                              (loop repeat r2
                                    do (funcall f p2 (%read-byte))
                                       (incf p2))))))))
        (declare (inline rle-component))
        (setf rle (new-rle-count r g b e))
        (assert (= rle length))
        (macrolet ((writer (a) `(ilambda (p v) (setf (aref ,a p) v))))
          (rle-component (writer rs))
          (rle-component (writer gs))
          (rle-component (writer bs)))
        (ecase type
          ((:rgb9-e5)
           (check-type destination (simple-array (unsigned-byte 32) 1))
           (locally (declare (type (simple-array (unsigned-byte 32) 1)
                                   destination))
             (rle-component
              (ilambda (p v)
                (declare (type (unsigned-byte 8) v)
                         (type (unsigned-byte 16) p))
                (if (zerop v)
                    (setf (aref destination (+ offset p)) 0)
                    ;; gl-exp = hdr-exp - 128 + 15
                    (let ((e (- v 113)))
                      (declare (type (signed-byte 9) e))
                      (cond
                        ;; 0<=biased-exponent<=31, write normally
                        ((not (logtest #b11100000 e))
                         (let ((w (ash (aref rs p) 1)))
                           (declare (type (unsigned-byte 32) w))
                           (setf (ldb (byte 9 9) w) (ash (aref gs p) 1))
                           (setf (ldb (byte 9 18) w) (ash (aref bs p) 1))
                           (setf (ldb (byte 5 27) w) e)
                           (setf (aref destination (+ offset p)) w)))

                        ;; exponent too small, clamp it and drop bits
                        ;; from mantissa
                        ((minusp e)
                         (cond
                           ((< e -9) ;; no bits left, write 0
                            (setf (aref destination (+ offset p)) 0))
                           (t
                            (let* ((r (aref rs p))
                                   (g (aref gs p))
                                   (b (aref bs p))
                                   (w2 0))
                              (declare (type (unsigned-byte 32) w2))
                              (setf (ldb (byte 9 0) w2) (ash r (1+ e)))
                              (setf (ldb (byte 9 9) w2) (ash g (1+ e)))
                              (setf (ldb (byte 9 18) w2) (ash b (1+ e)))
                              (setf (aref destination (+ offset p)) w2)))))

                        ;; exponent too large, clamp out of range channels
                        (t ;; (> e 31)
                         (decf e 31)
                         (cond
                           ((> e 9) ;; no bits left, write white
                            (setf (aref destination (+ offset p)) #xffffffff))
                           (t ;; scale and saturate components
                            (let* ((r (aref rs p))
                                   (g (aref gs p))
                                   (b (aref bs p))
                                   (w2 0))
                              (declare (type (unsigned-byte 32) w2))
                              (setf (ldb (byte 9 0) w2)
                                    (min 511 (ash r (1+ e))))
                              (setf (ldb (byte 9 9) w2)
                                    (min 511 (ash g (1+ e))))
                              (setf (ldb (byte 9 18) w2)
                                    (min 511 (ash b (1+ e))))
                              (setf (ldb (byte 5 27) w2) #b11111)
                              (setf (aref destination (+ offset p)) w2))))))))))))
          (:float
           (check-type destination (simple-array single-float 1))
           (locally (declare (type (simple-array single-float 1) destination))
             (rle-component (ilambda (p v)
                              (declare (type (unsigned-byte 8) v)
                                       (type (unsigned-byte 16) p))
                              (let ((p2 (+ offset (* p 3)))
                                    ;; 128 = bias of eponent in file,
                                    ;; 8 = # of bits of mantissa
                                    (e (expt 2.0 (- v (+ 128 8)))))
                                (declare (type single-float e))
                                (when (< p2 (- (length destination) 2))
                                  (setf (aref destination (+ p2 0))
                                        (* (aref rs p) e))
                                  (setf (aref destination (+ p2 1))
                                        (* (aref gs p) e))
                                  (setf (aref destination (+ p2 2))
                                        (* (aref bs p) e)))))))))))))

(defun read-scanline (%buf length destination &key (offset 0) (type :rgb9-e5))
  (multiple-value-bind (r g b e) (values
                                  (buf-read-byte %buf)
                                  (buf-read-byte %buf)
                                  (buf-read-byte %buf)
                                  (buf-read-byte %buf))
    (if (new-rle-p r g b e)
        (read-scanline-new %buf r g b e length destination offset type)
        (read-scanline-old %buf r g b e length destination offset type))))

(defclass hdr-file ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (headers :initarg :headers :accessor headers)
   (data :initarg :data :accessor data)
   (origin :initarg :origin :accessor origin :initform :upper-left)
   (gl-pixel-type :initarg :pixel-type :accessor gl-pixel-type)
   (gl-pixel-format :initarg :pixel-format :accessor gl-pixel-format)
   (gl-internal-format :initarg :internal-format :accessor gl-internal-format)
   (exposure :initarg :exposure :accessor exposure :initform nil)))

(defun read-hdr-stream (stream &key (format :rgb9-e5) y-up)
  "Reads a radiance HDR image from STREAM into a vector
of (UNSIGNED-BYTE 32) if FORMAT is :RGB9-E5, or SINGLE-FLOAT if format
is :FLOAT. If Y-UP is true, data is stored with bottom row of image
first, otherwise top row first."
  (let* ((b (make-instance 'buf :stream stream))
         (header (read-hdr-header b))
         (width (getf header :width))
         (height (getf header :height))
         (buf (ecase format

                ((:rgb9-e5)
                 (make-array (* width height)
                             :element-type '(unsigned-byte 32)
                             :initial-element #xffffffff))
                (:float
                 (make-array (* width height 3)
                             :element-type 'single-float
                             :initial-element most-positive-single-float))))
         (formats (ecase format
                    ((:rgb9-e5)
                     '(:internal-format :rgb9-e5
                       :pixel-format :rgb
                       :pixel-type :unsigned-int-5-9-9-9-rev))
                    (:float '(:internal-format :rgb32f
                              :pixel-format :rgb
                              :pixel-type :float))))
         ;; todo: handle other orientations of file data
         (flip y-up)
         (elements-per-pixel (ecase format ((:rgb9-e5) 1) (:float 3))))
    (assert (eql (getf header :origin) :upper-left))
    (if flip
        (loop for y from (1- height) downto 0
              do (read-scanline b width buf
                                :offset (* y width elements-per-pixel)
                                :type format))
        (loop for y below height
              do (read-scanline b width buf
                                :offset (* y width elements-per-pixel)
                                :type format)))
    (apply #'make-instance
           'hdr-file :width width :height height
           :data buf
           :exposure (or (getf header :exposure) 1.0)
           :origin (if y-up :lower-left :upper-left)
           :headers header
           formats)))

(defun read-hdr-file (file &key (format :rgb9-e5) y-up)
  "Reads a radiance HDR image from FILE into a vector
of (UNSIGNED-BYTE 32) if FORMAT is :RGB9-E5, or SINGLE-FLOAT if format
is :FLOAT. If Y-UP is true, data is stored with bottom row of image
first, otherwise top row first."
  (with-open-file (f file :element-type '(unsigned-byte 8))
    (read-hdr-stream f :format format :y-up y-up)))

(defun rgb9e5tofloat (a)
  (let ((d (make-array (* 3 (length a))
                       :element-type 'single-float
                       :initial-element most-positive-single-float)))
    (loop for i from 0 by 3
          for c across a
          for r = (ldb (byte 9 0) c)
          for g = (ldb (byte 9 9) c)
          for b = (ldb (byte 9 18) c)
          for .e = (ldb (byte 5 27) c)
          for e = (expt 2.0 (+ (- .e 15) -9))
          do (setf (aref d (+ i 0)) (* r e))
             (setf (aref d (+ i 1)) (* g e))
             (setf (aref d (+ i 2)) (* b e)))
    d))

;; current radiance code only write new rle or uncompressed, so match
;; that for now
#++
(defun write-scanline/old (stream data width offset)
  (let ((end (+ (* 3 width) offset)))
    (labels ((rle (r1 g1 b1 o)
               (loop for c from 1
                     for i from o below (- end 2)
                     for r = (aref data (+ i 0))
                     for g = (aref data (+ i 1))
                     for b = (aref data (+ i 2))
                     when (or (/= r r1) (/= g g1) (/= b b1))
                       return c
                     finally (return c)))
             (c (x)
               (min 255 (max 0 x)))
             (write-color (r g b)
               (let* ((e (round (log (max r g b) 2)))
                      (e2 (expt 2.0 e)))
                 (write-byte (c (round (/ r e2))) stream)
                 (write-byte (c (round (/ g e2))) stream)
                 (write-byte (c (round (/ b e2))) stream)
                 (write-byte (c e) stream))))
      (loop with x = offset
            for r = (aref data (+ x 0))
            for g = (aref data (+ x 1))
            for b = (aref data (+ x 2))
            for rle = (rle r g b (+ x 3))
            do (write-color r g b)
            when (> rle 4)
              do (incf x (* rle 3))
                 (loop
                   do (write-byte 1 stream)
                      (write-byte 1 stream)
                      (write-byte 1 stream)
                      (write-byte (ldb (byte 8 0) rle) stream)
                      (setf rle (ash rle -8))
                   while (> rle 0))
            else do (incf x 3)
            while (< x (- end 2))))))

(declaim (inline rgb2rgbe))
(defun rgb2rgbe (r g b)
  (if (= 0 r g b)
      (values 0 0 0 0)
      (flet ((c (x)
               (min 255 (max 0 x))))
        (let* ((e (1+ (floor (log (max r g b) 2))))
               (e2 (expt 2.0 (- e 8))))
          (values (c (round (/ r e2)))
                  (c (round (/ g e2)))
                  (c (round (/ b e2)))
                  (c (+ e 128)))))))

(defun write-scanline/old (stream data width offset compress)
  (declare (ignore compress))
  (let ((end (+ (* 3 width) offset)))
    (flet ((write-color (r g b)
             (multiple-value-bind (r g b e) (rgb2rgbe r g b)
               (write-byte r stream)
               (write-byte g stream)
               (write-byte b stream)
               (write-byte e stream))))
      (loop with x = offset
            for r = (aref data (+ x 0))
            for g = (aref data (+ x 1))
            for b = (aref data (+ x 2))
            do (write-color r g b)
               (incf x 3)
            while (< x (- end 2))))))

(defun write-scanline/new (stream data width offset)
  (let ((rs (make-array width :element-type '(unsigned-byte 8)))
        (gs (make-array width :element-type '(unsigned-byte 8)))
        (bs (make-array width :element-type '(unsigned-byte 8)))
        (es (make-array width :element-type '(unsigned-byte 8)))
        (end (+ (* 3 width) offset)))
    (labels ((runp (a o v)
               (loop for i from o below width
                     while (= (aref a i) v)
                     count 1))
             (norun (a o c)
               (loop
                 for rc = (min 128 c)
                 do (write-byte rc stream)
                    (when (> (+ o rc) width)
                      (error "bad run? ~s ~s ~s ~s~%" o c rc width))
                    (loop for i from o below (+ o rc)
                          do (write-byte (aref a i) stream))
                    (incf o rc)
                    (decf c rc)
                 while (plusp c)))
             (run (c v)
               (loop
                 for rc = (min 127 c)
                 do (write-byte (logior #x80 rc) stream)
                    (write-byte v stream)
                    (decf c rc)
                 while (plusp c)))
             (write-rle2 (a)
               (loop with norun = nil
                     with x = 0
                     for v = (aref a x)
                     for run = (runp a x v)
                     when (>= run 4)
                       do (when norun
                            (norun a norun (- x norun))
                            (setf norun nil))
                          (run run v)
                          (incf x run)
                     else do (unless norun (setf norun x))
                             (incf x)
                     while (< x width)
                     finally (when norun
                               (norun a norun (- x norun))))))
      (loop
        for x from offset by 3 below end
        for i below width
        for r = (aref data (+ x 0))
        for g = (aref data (+ x 1))
        for b = (aref data (+ x 2))
        do (multiple-value-bind (r g b e) (rgb2rgbe r g b)
             (setf (aref rs i) r)
             (setf (aref gs i) g)
             (setf (aref bs i) b)
             (setf (aref es i) e)))
      ;; rle2 scanline header
      (write-byte 2 stream)
      (write-byte 2 stream)
      (write-byte (ldb (byte 8 8) width) stream)
      (write-byte (ldb (byte 8 0) width) stream)
      ;; scanline contents
      (write-rle2 rs)
      (write-rle2 gs)
      (write-rle2 bs)
      (write-rle2 es))))


(defun write-scanline (stream data width offset compress)
  (if (and compress (< 8 width 32767))
      (write-scanline/new stream data width offset)
      (write-scanline/old stream data width offset compress)))

(defun write-hdr-stream (stream hdr &key uncompressed)
  "Write HDR to (UNSIGNED-BYTE 8) output stream STREAM, with 'new rle'
compression unless UNCOMPRESSED is true. HDR must have at least WIDTH,
HEIGHT, GL-PIXEL-TYPE and DATA values specified. If GL-PIXEL-TYPE is
:FLOAT, DATA should be a vector of R,G,B values. Otherwise, DATA is
assumed to be a vector of RGB9-E5 values."
  (let ((data (if (eql (gl-pixel-type hdr) :float)
                  (data hdr)
                  (rgb9e5tofloat (data hdr))))
        (w (width hdr))
        (h (height hdr))
        (exposure (exposure hdr)))
    (flet ((line (format &rest args)
             (let ((s (with-output-to-string (s)
                        (apply 'format s format args))))
               (write-sequence (babel:string-to-octets s :encoding :utf-8)
                               stream))))
      (line "#?RADIANCE~%")
      (line "# 3b-radiance-hdr~%")
      (line "FORMAT=32-bit_rle_rgbe~%")
      (when (and exposure (/= exposure 1.0))
        (line "EXPOSURE=~f~%" exposure))
      (line "~%-Y ~d +X ~d~%" h w))
    (if (eql (origin hdr) :upper-left)
        (loop for y from 0 by w
              repeat h
              do (write-scanline stream data w (* y 3) (not uncompressed)))
        (loop for y downfrom (* w (1- h)) by w
              repeat h
              do (write-scanline stream data w (* y 3) (not uncompressed))))))

(defun write-hdr-file (file hdr &key uncompressed
                                  (if-exists :error)
                                  (if-does-not-exist :create))
  "Write HDR to FILE, with 'new rle' compression unless UNCOMPRESSED
is true. HDR must have at least WIDTH, HEIGHT, GL-PIXEL-TYPE and DATA
values specified. If GL-PIXEL-TYPE is :FLOAT, DATA should be a vector
of R,G,B values. Otherwise, DATA is assumed to be a vector of RGB9-E5
values."
  (with-open-file (stream file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists
                          :if-does-not-exist if-does-not-exist)
    (write-hdr-stream stream hdr :uncompressed uncompressed)))


