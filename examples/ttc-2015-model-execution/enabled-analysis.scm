; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution enabled-analysis)
 (export
  specify-enabled-analysis)
 (import (rnrs) (rnrs mutable-pairs) (racr core) (ttc-2015-model-execution user-interface))
 
 (define (specify-enabled-analysis)
   (with-specification
    pn
    
    (ag-rule
     Enabled?
     
     (Arc
      (lambda (n)
        (define consumed (list))
        (define (find-consumable f)
          (ast-find-child
           (lambda (i n)
             (let ((enabled? (and (not (memq n consumed)) (f (->value n)) n)))
               (when enabled? (set! consumed (cons n consumed)))
               enabled?))
           (->Token* (Place n))))
        (set!
         consumed
         (map find-consumable (->consumers n)))
        (and (for-all (lambda (x) x) consumed) consumed)))
     
     (Transition
      (lambda (n)
        (and
         (not (ast-find-child (lambda (i n) (not (Enabled? n))) (->In n)))
         (fold-left
          (lambda (result n)
            (append result (Enabled? n)))
          (list)
          (->* (->In n))))))
     
     (AtomicPetrinet
      (lambda (n)
        (filter Enabled? (->* (->Transition* n)))))))))