cl-freetype2
============

This is a general Freetype 2 wrapper for Common Lisp using CFFI.  It's
geared toward both using Freetype directly by providing a simplified
API, as well as providing access to the underlying C structures and
functions for use with other libraries which may also use Freetype.


Overview
--------

Structures and accessors start with `FT-`:

* ft-face (struct), ft-face-num-faces, ft-face-face-index, ...
* ft-glyphslot (struct), ft-glyphslot-library, ft-glyphslot-face, ...

Exported API functions do _not_:

* `(new-face PATH)`
* `(set-pixel-sizes FACE WIDTH HEIGHT)`

Where Freetype would return a code, cl-freetype2 raises a condition:

    (defvar *face* (new-face "/bad/path")) => FreeType error: CANNOT-OPEN-RESOURCE

**NOTE 2011/12/06:** At this time, not all C API which return codes
have been wrapped, and even those which do raise a SIMPLE-ERROR.  This
is considered a bug.

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

**NOTE:** `fw-ptr` is clearly a terrible name, and is pending rename.


Documentation
-------------

Full API documentation is forthcoming, especially once the full API is
usefully complete. `:-P`


Example
-------

An example using the toy interface:

    (defparameter *face* (new-face "/usr/share/fonts/corefonts/times.ttf"))

    ;; Set the size to 24 points and 72 DPI
    (set-char-size *face* (* 24 64) 0 72 72)

    ;; Trivial output:
    (print-with-face *face* "Hello")

    ;; Output (if this doesn't display, make sure your encoding is
    ;; set to UTF-8):
                                                            ▒▒▓▓██      ▒▒▓▓██                            
    ████████████      ████████████                        ████████    ████████                            
      ▒▒████░░          ▒▒████░░                              ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████                                ████        ████                            
        ████              ████            ░░██████▓▓          ████        ████          ░░▓▓████▓▓░░      
        ████              ████          ▒▒██░░  ░░████        ████        ████        ░░██░░    ▓▓██▒▒    
        ██████████████████████        ░░██▒▒      ░░██▒▒      ████        ████        ██▒▒        ████░░  
        ████              ████        ▓▓██          ████      ████        ████      ▒▒██          ▒▒██▓▓  
        ████              ████        ██████████████████      ████        ████      ████            ████  
        ████              ████        ████                    ████        ████      ████            ████  
        ████              ████        ████░░          ░░      ████        ████      ████░░          ████  
        ████              ████        ▓▓██▓▓        ░░▓▓      ████        ████      ▓▓██▒▒          ██▒▒  
        ████              ████        ░░████▓▓    ░░██░░      ████        ████      ░░████        ░░██    
      ░░████░░          ░░████░░        ▓▓██████████▒▒      ▒▒████▒▒    ▒▒████▒▒      ▒▒████    ░░██░░    
    ████████████      ████████████        ▒▒██████░░      ████████████████████████      ░░▓▓████▓▓░░      


    ;; Trivial output:
    (print-with-face *face* "Hello" :right-left)
                                                                                                       
                                                                                                       
                                                                                                           
                             ▒▒▓▓██      ▒▒▓▓██                                                            
                           ████████    ████████                            ████████████      ████████████  
                               ████        ████                              ▒▒████░░          ▒▒████░░    
                               ████        ████                                ████              ████      
                               ████        ████                                ████              ████      
                               ████        ████                                ████              ████      
         ░░▓▓████▓▓░░          ████        ████          ░░██████▓▓            ████              ████      
       ░░██░░    ▓▓██▒▒        ████        ████        ▒▒██░░  ░░████          ████              ████      
       ██▒▒        ████░░      ████        ████      ░░██▒▒      ░░██▒▒        ██████████████████████      
     ▒▒██          ▒▒██▓▓      ████        ████      ▓▓██          ████        ████              ████      
     ████            ████      ████        ████      ██████████████████        ████              ████      
     ████            ████      ████        ████      ████                      ████              ████      
     ████░░          ████      ████        ████      ████░░          ░░        ████              ████      
     ▓▓██▒▒          ██▒▒      ████        ████      ▓▓██▓▓        ░░▓▓        ████              ████      
     ░░████        ░░██        ████        ████      ░░████▓▓    ░░██░░        ████              ████      
       ▒▒████    ░░██░░      ▒▒████▒▒    ▒▒████▒▒      ▓▓██████████▒▒        ░░████░░          ░░████░░    
         ░░▓▓████▓▓░░      ████████████████████████      ▒▒██████░░        ████████████      ████████████  
    

    ;; Trivial output:
    (print-with-face *face* "XY" :up-down)

    
    ░░████████████▓▓      ▓▓████████▓▓              
        ▒▒██████░░          ░░████░░                
          ░░████▒▒          ▒▒██                    
            ░░████        ░░██                      
              ▓▓████      ██░░                      
                ████▒▒  ▓▓▒▒                        
                ░░████▒▒▓▓                          
                  ▒▒████                            
                    ████▒▒                          
                  ▒▒▒▒████                          
                ░░▓▓  ▒▒████                        
                ██      ▓▓██▒▒                      
              ▓▓░░        ████░░                    
            ▓▓▒▒          ░░████                    
        ░░████              ██████░░                
      ██████████░░      ▓▓████████████              
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
      ████████████▒▒      ▒▒██████████              
        ▒▒████▓▓              ████░░                
          ░░████              ██                    
            ▒▒██▓▓          ▓▓░░                    
              ████░░      ░░▓▓                      
              ░░████      ██                        
                ▓▓██▒▒  ▒▒▒▒                        
                  ████░░██                          
                  ▒▒████░░                          
                    ████                            
                    ████                            
                    ████                            
                    ████                            
                    ████                            
                  ░░████░░                          
                ████████████

