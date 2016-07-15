; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr-meta core) (racr-meta testing))

(define (dummy-equation n) #t)

(define spec)

(define (create-ast-scheme)
  (set! spec (create-specification-2))
  (specify-start-symbol-2 spec 'A)
  (ast-rule-2 spec 'A->B)
  (ast-rule-2 spec 'B->A*-t1-D-t2)
  (ast-rule-2 spec 'C:B->D<D2)
  (ast-rule-2 spec 'D:E->)
  (ast-rule-2 spec 'E->)
  (let ((ast-scheme (racr-specification-2-ast-scheme spec)))
    (assert (att-value 'well-formed? ast-scheme)) ; Ensure, that the base grammar without attributes is correct!
    ast-scheme))

(define (run-error-cases)
  (let ((ast-scheme (create-ast-scheme))) ; Test attribution parser.
    (assert-exception (specify-attribute-2 spec 'attribute 1 '* #t dummy-equation #f))
    (assert-exception (specify-attribute-2 spec 'attribute 'A 1 #t dummy-equation #f))
    (assert-exception (specify-attribute-2 spec 'attribute 'A '* #t 1 #f))
    (assert-exception (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation 1))
    (assert-exception (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation (cons #f 1))))
  
  (let ((ast-scheme (create-ast-scheme))) ; Undefined rule context of synthesised attribute.
    (specify-attribute-2 spec 'attribute 'Undefined '* #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme))) ; Undefined rule context of inherited attribute.
    (specify-attribute-2 spec 'attribute 'Undefined 'B #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme))) ; Undefined right hand context of inherited attribute.
    (specify-attribute-2 spec 'attribute 'A 'Undefined #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme))) ; Right hand context of inherited attribute is terminal.
    (specify-attribute-2 spec 'attribute 'B 't1 #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme))))
  
  (let ((ast-scheme (create-ast-scheme))) ; Synthesised attribute redeclaration.
    (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme))))
  (let ((ast-scheme (create-ast-scheme))) ; Inherited attribute redeclaration.
    (specify-attribute-2 spec 'attribute 'A 'B #t dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A 'B #t dummy-equation #f)
    (assert (not (att-value 'well-formed? ast-scheme)))))

(define (run-correct-cases)
  (let ((ast-scheme (create-ast-scheme))) ; Test attribution parser.
    (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A '* #f dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A '* #t dummy-equation (cons #f eq?))
    (specify-attribute-2 spec 'attribute 'A '* #f dummy-equation (cons #f eq?))
    (specify-attribute-2 spec 'attribute 'A 'B #t dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A 'B #f dummy-equation #f)
    (specify-attribute-2 spec 'attribute 'A 'B #t dummy-equation (cons #f eq?))
    (specify-attribute-2 spec 'attribute 'A 'B #f dummy-equation (cons #f eq?)))
  
  (let ((ast-scheme (create-ast-scheme))) ; Test well-formedness analysis.
    (specify-attribute-2 spec 'a1 'A '* #t dummy-equation #f) ; Several synthesised attributes for the same AST rule.
    (specify-attribute-2 spec 'a2 'A '* #t dummy-equation #f)
    (assert (att-value 'well-formed? ast-scheme))
    (specify-attribute-2 spec 'a3 'A 'B #t dummy-equation #f) ; Several inherited attributes for the same context.
    (specify-attribute-2 spec 'a4 'A 'B #t dummy-equation #f)
    (assert (att-value 'well-formed? ast-scheme))
    (specify-attribute-2 spec 'a5 'B '* #t dummy-equation #f) ; Synthesised attribute shadowed by inheritance.
    (specify-attribute-2 spec 'a5 'C '* #t dummy-equation #f)
    (assert (att-value 'well-formed? ast-scheme))
    (specify-attribute-2 spec 'a6 'B 'A* #t dummy-equation #f) ; Inherited attribute shadowed by inheritance.
    (specify-attribute-2 spec 'a6 'C 'A* #t dummy-equation #f)
    (assert (att-value 'well-formed? ast-scheme))
    (specify-attribute-2 spec 'a7 'C 'D #t dummy-equation #f) ; Inherited attribute for subtype only.
    (specify-attribute-2 spec 'a8 'C '* #t dummy-equation #f) ; Synthesised attribute for subtype only.
    (specify-attribute-2 spec 'a8 'A 'B #t dummy-equation #f) ; Inherited attribute sometimes shadowed by synthesised.
    (specify-attribute-2 spec 'a5 'A 'B #t dummy-equation #f) ; Inherited attribute always shadowed by synthesised.
    (assert (att-value 'well-formed? ast-scheme))))

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(run-tests)
