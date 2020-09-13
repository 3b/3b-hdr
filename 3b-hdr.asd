(defsystem 3b-hdr
  :description "reader and writer for radiance HDR files"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria babel parse-number split-sequence)
  :in-order-to ((asdf:test-op (asdf:test-op 3b-hdr/test)))
  :serial t
  :components ((:file "package")
               (:file "hdr")))


(defsystem 3b-hdr/test
  :depends-on (3b-hdr nibbles parachute)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3b-hdr/test))
  :components ((:file "tests")))
