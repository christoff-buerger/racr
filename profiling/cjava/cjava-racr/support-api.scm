; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić, C. Bürger

#!r6rs

(library
 (cjava-racr support-api)
 (export
  specify-support-api)
 (import (rnrs) (racr core))
 
 (define specify-support-api
   (lambda (specification)
     (with-specification
      specification
      
      (define prepare-name
        (lambda (name)
          (map
           (lambda (part)
             (cond
               ((eq? part '*) part)
               ((eq? part '**) -1)
               (else
                (let ((str-part (symbol->string part)))
                  (if (eq? (string-ref str-part 0) #\*)
                      (string->number (substring str-part 1 (string-length str-part)))
                      part)))))
           name)))
      
      (ag-rule
       source-name
       (BindComposer
        (lambda (n)
          (prepare-name (ast-child 'sourcename n)))))
      
      (ag-rule
       target-name
       (BindComposer
        (lambda (n)
          (prepare-name (ast-child 'targetname n)))))))))