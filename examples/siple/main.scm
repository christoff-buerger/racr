; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple main)
 (export
  siple-specification
  interpret)
 (import
  (rnrs)
  (racr)
  (siple type)
  (siple state)
  (siple lexer)
  (siple ast)
  (siple parser)
  (siple access-support)
  (siple name-analysis)
  (siple type-analysis)
  (siple type-coercion)
  (siple control-flow-analysis)
  (siple well-formedness)
  (siple interpreter))
 
 (define interpret
   (lambda (input-file-name)
     (let* ((input-port (open-input-file input-file-name)))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (display
           (call/cc
            (lambda (k)
              (let* ((lexer (construct-lexer input-file-name input-port 4 k))
                     (parser (construct-parser lexer siple-specification #t k))
                     (ast (parser)))
                (weave-interpreter ast)
                (k (state-std-out ((ast-annotation ast 'interpret)))))))))
        (lambda ()
          (close-port input-port))))))
 
 ; Initialize SiPLE:
 (define siple-specification (create-specification))
 (when (= (specification->phase siple-specification) 1)
   (specify-ast siple-specification)
   (compile-ast-specifications siple-specification 'CompilationUnit)
   (specify-access-support siple-specification)
   (specify-name-analysis siple-specification)
   (specify-type-analysis siple-specification)
   (specify-type-coercion siple-specification)
   (specify-control-flow-analysis siple-specification)
   (specify-well-formedness siple-specification)
   (compile-ag-specifications siple-specification)))