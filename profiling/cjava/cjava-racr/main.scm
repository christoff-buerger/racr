; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr main)
 (export
  compile)
 (import
  (rnrs)
  (racr core)
  (cjava-racr exception-api)
  (cjava-racr ast)
  (cjava-racr lexer)
  (cjava-racr parser)
  (cjava-racr unparser)
  (cjava-racr lexer-cl)
  (cjava-racr parser-cl)
  (cjava-racr support-api)
  (cjava-racr name-analysis)
  (cjava-racr compositions)
  (cjava-racr well-formedness))
 
 (define compile
   (lambda (composition-mode composition-recipe . cjava-programs)
     (let ((ast (apply parse composition-recipe cjava-programs)))
       (define ensure-wellformedness
         (lambda ()
           (unless (att-value 'correct? ast)
             (throw-cjava-racr-exception "System wellformedness violated."))))
       (cond
         ((string=? composition-mode "always")
          (do-compositions ast ensure-wellformedness)
          (ensure-wellformedness))
         ((string=? composition-mode "once")
          (do-compositions ast (lambda () #t))
          (ensure-wellformedness))
         ((string=? composition-mode "never")
          (do-compositions ast (lambda () #t)))
         (else (throw-cjava-racr-exception "Unknown composition mode.")))
       (unparse ast))))
 
 (define parse
   (lambda (composition-recipe . cjava-programs)
     (create-ast
      cjava-racr-specification
      'CompilationUnit
      (list
       (create-ast-list
        (map
         (lambda (src)
           (let ((src-input-port
                  (open-file-input-port
                   src
                   (file-options)
                   (buffer-mode line)
                   (make-transcoder (utf-8-codec)))))
             (dynamic-wind
              (lambda () #f)
              (lambda ()
                ((construct-parser (construct-lexer src src-input-port 4) cjava-racr-specification)))
              (lambda ()
                (close-port src-input-port)))))
         cjava-programs))
       (let ((recipe-input-port
              (open-file-input-port
               composition-recipe
               (file-options)
               (buffer-mode line)
               (make-transcoder (utf-8-codec)))))
         (dynamic-wind
          (lambda () #f)
          (lambda ()
            ((construct-parser-cl (construct-lexer-cl composition-recipe recipe-input-port 4) cjava-racr-specification)))
          (lambda ()
            (close-port recipe-input-port))))))))
 
 (define unparse
   (lambda (compilation-unit)
     (ast-for-each-child
      (lambda (i n)
        (let ((cjava-output-port
               (open-file-output-port
                (ast-child 'srcfile n)
                (file-options no-fail)
                (buffer-mode block)
                (make-transcoder (utf-8-codec)))))
          (dynamic-wind
           (lambda () #f)
           (lambda ()
             (pretty-print n cjava-output-port))
           (lambda ()
             (close-port cjava-output-port)))))
      (ast-child 'Body compilation-unit))))
 
 ; Initialization:
 (define cjava-racr-specification (create-specification))
 (when (= (specification->phase cjava-racr-specification) 1)
   (specify-ast cjava-racr-specification)
   (compile-ast-specifications cjava-racr-specification 'CompilationUnit)
   (specify-support-api cjava-racr-specification)
   (specify-name-analysis cjava-racr-specification)
   (specify-compositions cjava-racr-specification)
   (specify-well-formedness cjava-racr-specification)
   (compile-ag-specifications cjava-racr-specification)))