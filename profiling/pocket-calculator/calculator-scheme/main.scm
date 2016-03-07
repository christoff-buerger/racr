; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger, D. Langner

#!r6rs

(library
 (calculator-scheme main)
 (export :Calculator :Definition :Addition :Multiplication :Number :Constant
         ->Definition* ->Expression ->Op1 ->Op2 ->name ->value
         =lookup =eval
         number->const random random-integer make-profiling-ast)
 (import (rnrs) (racr core) (racr testing))
 
 (define spec (create-specification))
 
 ; AST and attribute accessors and AST constructors:
 (define (->Definition* n)         (ast-child 'Definition* n))
 (define (->Expression n)          (ast-child 'Expression n))
 (define (->Op1 n)                 (ast-child 'Op1 n))
 (define (->Op2 n)                 (ast-child 'Op2 n))
 (define (->name n)                (ast-child 'name n))
 (define (->value n)               (ast-child 'value n))
 (define (=lookup n name)          (att-value 'lookup n name))
 (define (=eval n)                 (att-value 'eval n))
 (define (:Calculator defs expr)   (create-ast spec 'Calculator (list defs expr)))
 (define (:Definition name value)  (create-ast spec 'Definition (list name value)))
 (define (:Addition op1 op2)       (create-ast spec 'Addition (list op1 op2)))
 (define (:Multiplication op1 op2) (create-ast spec 'Multiplication (list op1 op2)))
 (define (:Number value)           (create-ast spec 'Number (list value)))
 (define (:Constant name)          (create-ast spec 'Constant (list name)))
 (define (number->const i)         (string-append "d" (number->string i)))
 
 ; Random expression generation:
 
 (define random
   (let ((a 69069)
         (c 1)
         (m (expt 2 32))
         (seed 19380110))
     (case-lambda
       ((new-seed) (set! seed new-seed) (/ seed m))
       (()         (set! seed (mod (+ (* seed a) c) m)) (/ seed m)))))
 
 (define random-integer
   (case-lambda
     ((hi)       (floor (* (random) hi)))
     ((lo hi)    (+ lo (floor (* (random) (- hi lo)))))))
 
 (define (make-profiling-ast nodes constants)
   (define (make-definitions)
     (do ((i (- constants 1) (- i 1))
          (defs (list) (cons (:Definition (number->const i) (/ i 10.0)) defs)))
       ((< i 0) (create-ast-list defs))))
   (define (new-node)
     (define type (if (< (random) (/ 1 2)) :Addition :Multiplication))
     (type (create-ast-bud) (create-ast-bud)))
   (define (add-node n)
     (define c (ast-child (if (< (random) (/ 1 2)) 'Op1 'Op2) n))
     (if (ast-bud-node? c) (rewrite-subtree c (new-node)) (add-node c)))
   (define (initialise-leafes n)
     (define (initialise-leaf n)
       (cond
         ((not (ast-bud-node? n))
          (initialise-leafes n))
         ((< (random) (/ 1 2))
          (rewrite-subtree n (:Number (+ (random-integer 1 10) 0.0))))
         (else
          (rewrite-subtree n (:Constant (number->const (random-integer 0 constants)))))))
     (initialise-leaf (ast-child 'Op1 n))
     (initialise-leaf (ast-child 'Op2 n)))
   (let ((ast (new-node)))
     (do ((i 1 (+ i 1))) ((= i nodes)) (add-node ast))
     (initialise-leafes ast)
     (:Calculator (make-definitions) ast)))
 
 ; Language definition:
 
 (with-specification
  spec
  
  (ast-rule 'Calculator->Definition*-Expression)
  (ast-rule 'Definition->name-value)
  (ast-rule 'Expression->)
  (ast-rule 'BinaryExpression:Expression->Expression<Op1-Expression<Op2)
  (ast-rule 'Addition:BinaryExpression->)
  (ast-rule 'Multiplication:BinaryExpression->)
  (ast-rule 'Number:Expression->value)
  (ast-rule 'Constant:Expression->name)
  (compile-ast-specifications 'Calculator)
  
  (ag-rule
   eval ; Value of expression.
   (Calculator     (lambda (n) (=eval (->Expression n))))
   (Addition       (lambda (n) (+ (=eval (->Op1 n)) (=eval (->Op2 n)))))
   (Multiplication (lambda (n) (* (=eval (->Op1 n)) (=eval (->Op2 n)))))
   (Constant       (lambda (n) (->value (=lookup n (->name n)))))
   (Number         ->value))
  
  (ag-rule
   lookup ; Find definition of given name (#f, if undefined).
   (Calculator
    (lambda (n name)
      (ast-find-child
       (lambda (i n) (string=? (->name n) name))
       (->Definition* n))))))
 
 (compile-ag-specifications spec))