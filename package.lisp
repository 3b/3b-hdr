(defpackage #:3b-hdr
  (:use :cl)
  (:export
   #:read-hdr-file
   #:write-hdr-file

   #:read-hdr-stream
   #:write-hdr-stream

   #:width
   #:height
   #:headers
   #:data
   #:origin
   #:gl-pixel-type
   #:gl-pixel-format
   #:gl-internal-format
   #:exposure
   #:hdr-file))
