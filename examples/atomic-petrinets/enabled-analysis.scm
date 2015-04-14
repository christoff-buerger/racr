; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets enabled-analysis)
 (export specify-enabled-analysis)
 (import (rnrs) (racr core) (atomic-petrinets query-support))
 
 (define (specify-enabled-analysis)
   (with-specification
    pn
    
    (ag-rule
     enabled?
     
     (Arc
      (lambda (n)
        (define consumed (list))
        (define (find-consumable f)
          (ast-find-child
           (lambda (i n)
             (let ((enabled? (and (not (memq n consumed)) (f (->value n)) n)))
               (when enabled? (set! consumed (cons n consumed)))
               enabled?))
           (->Token* (=place n))))
        (call/cc
         (lambda (abort)
           (fold-left
            (lambda (result f)
              (define consumed? (find-consumable f))
              (if consumed? (cons consumed? result) (abort #f)))
            (list)
            (->consumers n))))))
        ;(set!
        ; consumed
        ; (map find-consumable (->consumers n)))
        ;(and (for-all (lambda (x) x) consumed) consumed)))
     
     (Transition
      (lambda (n)
        ;(define result (list))
        ;(and
        ; (not
        ;  (ast-find-child
        ;   (lambda (i n)
        ;     (let ((enabled? (=enabled? n)))
        ;       (and enabled? (begin (set! result (append result enabled?)) #f))))
        ;   (->In n)))
        ; result)
        (and
         (not (ast-find-child (lambda (i n) (not (=enabled? n))) (->In n)))
         (fold-left
          (lambda (result n)
            (append result (=enabled? n)))
          (list)
          (=in-arcs n)))))))))