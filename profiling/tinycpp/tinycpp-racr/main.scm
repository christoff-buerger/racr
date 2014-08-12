; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr main)
 (export
  compile)
 (import
  (rnrs)
  (racr core)
  (tinycpp-racr exception-api)
  (tinycpp-racr ast)
  (tinycpp-racr lexer)
  (tinycpp-racr parser)
  (tinycpp-racr unparser)
  (tinycpp-racr support-api)
  (tinycpp-racr name-analysis)
  (tinycpp-racr normalization)
  (tinycpp-racr well-formedness))
 
 (define compile
   (lambda (program)
     (let ((ast (parse program)))
       (transform-to-normalform ast)
       (unless (att-value 'correct? ast)
         (throw-tinycpp-racr-exception "ERROR: Program not well-formed."))
       (unparse ast))))
 
 (define parse
   (lambda (program)
     (let ((src-input-port
            (open-file-input-port
             program
             (file-options)
             (buffer-mode line)
             (make-transcoder (utf-8-codec)))))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          ((construct-parser (construct-lexer program src-input-port 4) specification)))
        (lambda ()
          (close-port src-input-port))))))
 
 (define unparse
   (lambda (compilation-unit)
     (let ((res-output-port
            (open-file-output-port
             (ast-child 'sourcefile compilation-unit)
             (file-options no-fail)
             (buffer-mode block)
             (make-transcoder (utf-8-codec)))))
       (dynamic-wind
        (lambda () #f)
        (lambda ()
          (pretty-print compilation-unit res-output-port))
        (lambda ()
          (close-port res-output-port))))))
 
 ; Initialization:
 (define specification (create-specification))
 (when (= (specification->phase specification) 1)
   (specify-ast specification)
   (compile-ast-specifications specification 'CompilationUnit)
   (specify-support-api specification)
   (specify-name-analysis specification)
   (specify-normalization specification)
   (specify-well-formedness specification)
   (compile-ag-specifications specification)))