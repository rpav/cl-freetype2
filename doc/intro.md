{anchor platform}

Platform
--------

Confirmed working:

* SBCL
* Clozure
* CLISP (with CVS libffcall only, otherwise callbacks segfault)

Confirmed not working:

* ECL (CFFI doesn't load)
* ABCL (CFFI loads, but can't grovel?)

{anchor overview}

Overview
--------

The main functionality of cl-freetype2 is divided into three packages:

* `freetype2` (alias `ft2`) contains the main API intended for user
  consumption.  Some care needs to be taken to not discard certain
  references, but this should for the most part be fairly idiomatic.
  [See the reference](#REFERENCE-FREETYPE2).

* `freetype2-types` contains C type definitions and accessors.
  Structure contents are not automatically converted, but accessors
  convert between foreign and native types on demand.  Types obtained
  from `freetype2:` functions are automatically collected, but
  references obtained from structure slots do _not_.
  [See the reference](cl-freetype2-types.html#REFERENCE-CL-FREETYPE2-TYPES).

* `freetype2-ffi` contains C API definitions.  It is generally not
  necessary to call these, but it may be to effect some special
  functionality or integrate with other C libraries which use
  FreeType.
  [See the reference](cl-freetype2-ffi.html#REFERENCE-CL-FREETYPE2-FFI).

{anchor types}
