### Read/Write Radiance [shared-exponent RGBE HDR image files](https://floyd.lbl.gov/radiance/refer/filefmts.pdf#%5B%7B%22num%22%3A114%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22FitH%22%7D%2C541%5D)


#### Reading

Read with `(READ-HDR-FILE FILE-NAME &key FORMAT Y-UP)` or `(READ-HDR-STREAM STREAM &key FORMAT Y-UP)`.

`FILE-NAME` is a pathname designator for an HDR file.

`STREAM` is an open `(unsigned-byte 8)` input stream.

`FORMAT` is either `:RGB9-E5`(default) or `:FLOAT`. If it is `:FLOAT`, data will be returned as `single-floats`, if `:RGB9-E5` it will be returned as `(unsigned-byte 32)` containing GL-style `RGB9-E5` data.

`Y-UP` specifies that data should be returned as GL-style lower-left origin (bottom row first), otherwise it is returned with upper-left origin (top-row first).

Both functions return an `HDR-FILE` object with following accessors:

`WIDTH` = width of image in pixels

`HEIGHT` = height of image in rows

`DATA` = vector of `(UNSIGNED-BYTE 32)` with 1 element per pixel or `SINGLE-FLOAT` with 3 elements per pixel, depending on requested `FORMAT` when loading.

`ORIGIN` = `:UPPER-LEFT` or `:LOWER-LEFT` depending on requested `Y-UP` when loading.

`GL-PIXEL-TYPE`, `GL-PIXEL-FORMAT`, `GL-INTERNAL-FORMAT` = values suitable to pass to cl-opengl when creating/uploading textures from data, `:unsigned-int-5-9-9-9-rev :rgb :rgb9-e5` or `:float :rgb :rgb32f` depending on `FORMAT`

`EXPOSURE` = (product of) `EXPOSURE` header value(s) in HDR file, or 1.0 if none specified. Usually 1.0.



#### Writing

Write with `(WRITE-HDR-FILE FILE-NAME HDR &key UNCOMPRESSED IF-EXISTS IF-DOES-NOT-EXIST)` or `(WRITE-HDR-STREAM STREAM HDR &key UNCOMPRESSED)`

`FILE-NAME` is a pathname designator for an HDR file to be written.

`STREAM` is an open `(unsigned-byte 8)` output stream.

`HDR` is an `HDR-FILE` object, with at least `WIDTH`, `HEIGHT`, `DATA`, `GL-PIXEL-TYPE`, and `ORIGIN` specified.

If `UNCOMPRESSED` is true, write scanlines as uncompressed RGBE instead of new-style RLE-compressed data.

`IF-EXISTS` (default :error) and `IF-DOES-NOT-EXIST` (default :create) are passed to `CL:OPEN`


