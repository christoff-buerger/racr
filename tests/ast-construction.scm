; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr-meta core) (racr-meta testing))

(define (create-test-language)
  (define spec (create-specification-2))
  (ast-rule-2 spec 'A->B)
  (ast-rule-2 spec 'B->A*-t1-D-t2)
  (ast-rule-2 spec 'C:B->D<D2)
  (ast-rule-2 spec 'D:E->)
  (ast-rule-2 spec 'E->E*)
  (specify-attribute-2 spec 'erroneous-attribute 'E '* #t (lambda (n) (create-ast-list-2 (list n))) #f)
  (specify-attribute-2 spec 'erroneous-attribute 'B '* #t (lambda (n) (create-ast-2 spec 'A (list n))) #f)
  (specify-start-symbol-2 spec 'A)
  (compile-specification-2 spec)
  spec)

(define (run-error-cases)
  (define spec (create-test-language))
  (assert-exception ; Terminal as list element.
   (create-ast-list-2 (list 'terminal)))
  (assert-exception ; Nested lists.
   (create-ast-list-2 (list (create-ast-list-2 (list)))))
  (assert-exception ; Unknown non-terminal.
   (create-ast-2 spec 'Undefined (list)))
  (assert-exception ; Insufficient many candidates.
   (create-ast-2 spec 'A (list)))
  (assert-exception ; To many candidates.
   (create-ast-2 spec 'A (list (create-ast-bud) 1)))
  (assert-exception ; Unexpected terminal.
   (create-ast-2 spec 'A (list 'terminal)))
  (assert-exception ; Unexpected non-terminal.
   (create-ast-2 spec 'A (list (create-ast-2 spec 'E (list (create-ast-bud))))))
  (assert-exception ; Unexpected list.
   (create-ast-2 spec 'A (list (create-ast-list-2 (list)))))
  (assert-exception ; Unexpected non-list (terminal).
   (create-ast-2 spec 'E (list 'terminal)))
  (assert-exception ; Unexpected non-list (non-terminal).
   (create-ast-2 spec 'E (list (create-ast-2 spec 'E (list (create-ast-bud))))))
  (assert-exception ; List does not fit because of element of wrong type.
   (create-ast-2 spec 'E (list (create-ast-2 spec 'A (list (create-ast-bud))))))
  (let ((ast (create-ast-bud))) ; Candidate already part of AST (list construction).
    (assert-exception (create-ast-list-2 ast))
    (assert-exception (create-ast-list-2 ast)))
  (let ((ast (create-ast-bud))) ; Candidate already part of AST (non-list construction).
    (assert-exception (create-ast-2 'A (list ast)))
    (assert-exception (create-ast-2 'A (list ast))))
  (assert-exception ; Candidate in evaluation (list construction).
   (att-value 'erroneous-attribute (create-ast-2 'E (list (create-ast-bud)))))
  (assert-exception ; Candidate in evaluation (non-list construction).
   (att-value 'erroneous-attribute (create-ast-2 'B (list (create-ast-bud) #f (create-ast-bud) #f)))))

(define (run-correct-cases)
  (define spec (create-test-language))
  (create-ast-bud) ; Create bud node.
  (create-ast-list-2 (list)) ; Create list node.
  (create-ast-list-2 (list (create-ast-bud))) ; Create list of bude nodes.
  (create-ast-2 spec 'E (list (create-ast-bud))) ; Bud node child instead of list.
  (create-ast-2 spec 'A (list (create-ast-bud))) ; Bud node child instead of non-list.
  (create-ast-2 ; Child of non-inherited type.
   spec
   'A
   (list (create-ast-2 spec 'B (list (create-ast-bud) #t (create-ast-bud) #t))))
  (create-ast-2 ; Child of inherited type.
   spec
   'A
   (list
    (create-ast-2 spec 'C (list (create-ast-bud) #t (create-ast-bud) #t (create-ast-bud)))))
  (create-ast-2 ; Child of required inherited type.
   spec
   'B
   (list
    (create-ast-bud)
    #t
    (create-ast-2 spec 'D (list (create-ast-bud)))
    #t))
  (create-ast-2 spec 'E (list (create-ast-list-2 (list)))) ; Empty list child.
  (create-ast-2 spec 'E (list (create-ast-list-2 (list (create-ast-bud))))) ; List child with bud node elements.
  (create-ast-2 ; List child with non-bud node elements of non-inherited type.
   spec
   'E
   (list
    (create-ast-list-2
     (list
      (create-ast-2 spec 'E (list (create-ast-bud)))))))
  (create-ast-2 ; List child with non-bud node elements of inherited type.
   spec
   'E
   (list
    (create-ast-list-2
     (list
      (create-ast-2 spec 'D (list (create-ast-bud))))))))

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(run-tests)