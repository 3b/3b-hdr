(defsystem 3b-hdr
  :description "reader and writer for radiance HDR files"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria babel parse-number split-sequence)
  :serial t
  :components ((:file "package")
               (:file "hdr")))


(defsystem 3b-hdr/test
  :depends-on (3b-hdr nibbles parachute)
  :serial t
  :components ((:file "tests")))
