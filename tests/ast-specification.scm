; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr-meta core) (racr-meta testing))

(define spec)

(define (create-ast-scheme startsymbol)
  (set! spec (create-specification-2))
  (specify-start-symbol-2 spec startsymbol)
  (racr-specification-2-ast-scheme spec))

(define (run-error-cases)
  (let ((spec (create-specification-2))) ; Test AST rule parser.
    (assert-exception (ast-rule-2 spec 'a->))
    (assert-exception (ast-rule-2 spec 'A))
    (assert-exception (ast-rule-2 spec 'A:))
    (assert-exception (ast-rule-2 spec 'A:a->))
    (assert-exception (ast-rule-2 spec 'A->a*))
    (assert-exception (ast-rule-2 spec 'A->a<b))
    (assert-exception (ast-rule-2 spec 'A->a*<b))
    (assert-exception (ast-rule-2 spec 'A->A-))
    (assert-exception (ast-rule-2 spec 'A->a-))
    (assert-exception (ast-rule-2 spec 'A->A*-))
    (assert-exception (ast-rule-2 spec 'A->A<myA-))
    (assert-exception (ast-rule-2 spec 'A->A*<myA-))
    (assert-exception (ast-rule-2 spec 'A->a*-))
    (assert-exception (ast-rule-2 spec 'A->a<b-))
    (assert-exception (ast-rule-2 spec 'A->a*<b-)))
  
  (let ((ast-scheme (create-ast-scheme 'B))) ; Undefined start symbol.
    (ast-rule-2 spec 'A->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; Undefined supertype.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B:C->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; Undefined symbol.
    (ast-rule-2 spec 'A->B)
    (assert (not (att-value 'well-formed? ast-scheme))))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; No unique context names for terminals.
    (ast-rule-2 spec 'A->a-a)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; No unique context names for non-terminals.
    (ast-rule-2 spec 'A->B-B)
    (ast-rule-2 spec 'B->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; No unique context names for lists.
    (ast-rule-2 spec 'A->B*-B*)
    (ast-rule-2 spec 'B->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; No unique context name between terminal & non-terminal.
    (ast-rule-2 spec 'A->a-B*<a)
    (ast-rule-2 spec 'B->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; No unique context names because of inheritance.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B->a)
    (ast-rule-2 spec 'C:B->a)
    (assert (not (att-value 'well-formed? ast-scheme))))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; Non-productive rule.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B->C)
    (ast-rule-2 spec 'C->A)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme 'A))) ; Non-productive rule because of inheritance.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B:C->)
    (ast-rule-2 spec 'C->A)
    (assert (not (att-value 'well-formed? ast-scheme))))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; Non-derivable non-terminal.
    (ast-rule-2 spec 'A->)
    (ast-rule-2 spec 'B->)
    (assert (not (att-value 'well-formed? ast-scheme))))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; Cyclic inheritance.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B:C->a)
    (ast-rule-2 spec 'C:D->b)
    (ast-rule-2 spec 'D:B->c)
    (assert (not (att-value 'well-formed? ast-scheme)))))

(define (run-correct-cases)
  (let ((spec (create-specification-2))) ; Test AST rule parser.
    (ast-rule-2 spec 'A->)
    (ast-rule-2 spec 'A:A->)
    (ast-rule-2 spec 'A->a)
    (ast-rule-2 spec 'A->A)
    (ast-rule-2 spec 'A->A*)
    (ast-rule-2 spec 'A->A<myA)
    (ast-rule-2 spec 'A->A*<myA2)
    (ast-rule-2 spec 'A->a-A)
    (ast-rule-2 spec 'A->A*-A*<A2-b)
    (assert ; Identifiers and right hands are lexed and parsed in correct order.
     (equal?
      (list 't1 'Foo 't2 'Bar)
      (map
       (lambda (symbol)
         (ast-child 'name symbol))
       (ast-children (ast-child 'rhand (ast-rule-2 spec 'A->t1-Foo-t2-Bar)))))))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; Test well-formedness analysis.
    (ast-rule-2 spec 'A->B)
    (ast-rule-2 spec 'B->A*-t1-D-t2)
    (ast-rule-2 spec 'C:B->D<D2)
    (ast-rule-2 spec 'D:E->)
    (ast-rule-2 spec 'E->)
    (assert (att-value 'well-formed? ast-scheme)))
  
  (let ((ast-scheme (create-ast-scheme 'A))) ; Test well-formedness analysis.
    (ast-rule-2 spec 'F->)
    (ast-rule-2 spec 'E:A->)
    (ast-rule-2 spec 'C:A->A*-t2)
    (ast-rule-2 spec 'B:A->A-t2)
    (ast-rule-2 spec 'A->t1-F)
    (assert (att-value 'well-formed? ast-scheme))))

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(run-tests)