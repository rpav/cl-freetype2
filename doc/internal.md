{anchor internals}

Internals
=========

FreeType2 is not something generally used directly in most application
code, but there are a number of libraries which use it.  Thus
cl-freetype2 has been written with integration in mind.  This section
should be a guide for the underlying implementation that may be useful
inu such cases.

{anchor types}

Types
-----

Freetype types are represented by a DEFSTRUCT-wrapped CFFI pointer.
These are freed via TRIVIAL-FINALIZER when no references **to the
wrapper** remain.  If you otherwise hand the pointer off to something
else which retains it, you **must** keep a reference to the wrapper
for the life of the pointer.

There is no other translation or conversion until accessors are called.

    (defvar *face* (new-face *valid-face-path*)) => #<FT-FACE "Name Family" {#x00844C90}>

The pointer can be accessed with FW-PTR:

    ;; The format will vary depending on your Lisp, of course:
    (fw-ptr *face*) => #.(SB-SYS:INT-SAP #X00844C90)

(**NOTE:** `fw-ptr` is clearly a terrible name, and is pending
rename.)

{anchor testing}

Testing
-------

Testing is done via 5am and the `:cl-freetype2-tests` package, which
resides in `t/`.  To just run tests, the following is sufficient:

    (asdf:test-system :cl-freetype2)

All tests should pass.

{anchor porting}

Porting
-------

If CFFI loads, and can grovel, most of cl-freetype2 should work.  You
may need to look in `src/ffi/cffi-cwrap.lisp` and tweak the
definitions of `FINALIZE` and `FOREIGN-WRAPPER`.

`FINALIZE` has two implementations: one that simply calls
`TG:FINALIZE`, and one that tracks references to finalizers to ensure
call order.  Proper cleanup order is critical, and out-of-order calls
will result in memory faults.

`FOREIGN-WRAPPER` defaults the `PTR` slot to `(cffi:null-pointer)`,
but depending on the implementation, this may or may not be an
immediate value.

Running the tests should validate the majority of the API.  Callbacks
may be an issue; these are used for outlines, which are part of the
unit tests.
