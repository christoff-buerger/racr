; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple main)
 (export
  siple-specification
  siple-interpret)
 (import
  (rnrs)
  (racr core)
  (siple exception-api)
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
 
 (define siple-interpret
   (case-lambda
     (()
      (display "Enter SiPLE Program (Finish with EOF Character).\n")
      (let ((program (get-string-all (current-input-port))))
        (display "Program Entered. Process Program...\n")
        (siple-interpret (open-string-input-port program))))
     ((input)
      (siple-interpret input (current-output-port)))
     ((input output-port)
      (let* ((input-port
              (cond
                ((string? input)
                 (open-input-file input))
                ((input-port? input)
                 input)
                (else (throw-siple-exception "Unknown input; File name or input-port expected.")))))
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (let* ((lexer (construct-lexer input-port input-port 4))
                  (parser (construct-parser lexer siple-specification #t))
                  (ast (parser)))
             (weave-interpreter ast)
             ((ast-annotation ast 'interpret) output-port)
             ; Arbitrary procedure application that does not return anything.
             ; Avoids printing the interpretation's result state:
             (let ((dummy-var #f)) (set! dummy-var #f))))
         (lambda ()
           (close-port input-port)))))))
 
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