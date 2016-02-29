; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger, D. Langner

#!r6rs

(library
 (calculator-scheme main)
 (export random random-integer number->const make-profiling-ast)
 (import (rnrs) (racr core) (racr testing))
 
 ;;; Support functions:
 
 (define random
   (let ((a 69069)
         (c 1)
         (m (expt 2 32))
         (seed 19380110))
     (case-lambda
       ((new-seed) (set! seed new-seed) (/ seed m))
       (()         (set! seed (mod (+ (* seed a) c) m)) (/ seed m)))))
 
 (define random-integer
   (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
     (case-lambda
       ((hi)       (floor (* (random) hi)))
       ((lo hi)    (+ lo (floor (* (random) (- hi lo))))))))
 
 ;;; Random expression generation:
 
 (define spec (create-specification))
 
 (define (:Root defs expr) (create-ast spec 'Root (list defs expr)))
 (define (:Def name value) (create-ast spec 'Def (list name value)))
 (define (:AddExp op1 op2) (create-ast spec 'AddExp (list op1 op2)))
 (define (:MulExp op1 op2) (create-ast spec 'AddExp (list op1 op2)))
 (define (:Number value)   (create-ast spec 'Number (list value)))
 (define (:Const name)     (create-ast spec 'Const (list name)))
 (define (number->const i) (string-append "d" (number->string i)))
 
 (define (make-profiling-ast nodes constants)
   (define (make-definitions)
     (do ((i 0 (+ i 1))
          (defs (list) (cons (:Def (number->const i) (/ i 10.0)) defs)))
       ((= i constants) (create-ast-list defs))))
   (define (new-node)
     (define type (if (< (random) (/ 1 2)) :AddExp :MulExp))
     (type (create-ast-bud) (create-ast-bud)))
   (define (add-node n)
     (define c (ast-child (if (< (random) (/ 1 2)) 'A 'B) n))
     (if (ast-bud-node? c) (rewrite-subtree c (new-node)) (add-node c)))
   (define (initialise-leafes n)
     (define (initialise-leaf n)
       (cond
         ((not (ast-bud-node? n))
          (initialise-leafes n))
         ((> (random) (/ 1 2))
          (rewrite-subtree n (:Number (random-integer 1 10))))
         (else
          (rewrite-subtree n (:Const (number->const (random-integer 0 constants)))))))
     (initialise-leaf (ast-child 'A n))
     (initialise-leaf (ast-child 'B n)))
   (let ((ast (new-node)))
     (do ((i 1 (+ i 1))) ((= i nodes)) (add-node ast))
     (initialise-leafes ast)
     (:Root (make-definitions) ast)))
 
 ;;; Language definition:
 
 (with-specification
  spec
  
  (ast-rule 'Root->Def*-Exp)
  (ast-rule 'Def->name-value)
  (ast-rule 'Exp->)
  (ast-rule 'BinExp:Exp->Exp<A-Exp<B)
  (ast-rule 'AddExp:BinExp->)
  (ast-rule 'MulExp:BinExp->)
  (ast-rule 'Number:Exp->value)
  (ast-rule 'Const:Exp->name)
  (compile-ast-specifications 'Root)
  
  (ag-rule
   Eval
   (Root
    (lambda (n)
      (att-value 'Eval (ast-child 'Exp n))))
   (AddExp
    (lambda (n)
      (+
       (att-value 'Eval (ast-child 'A n))
       (att-value 'Eval (ast-child 'B n)))))
   (MulExp
    (lambda (n)
      (*
       (att-value 'Eval (ast-child 'A n))
       (att-value 'Eval (ast-child 'B n)))))
   
   (Number
    (lambda (n)
      (ast-child 'value n)))
   
   (Const
    (lambda (n)
      (ast-child 'value (att-value 'Lookup n (ast-child 'name n))))))
  
  (ag-rule
   Lookup
   (Root
    (lambda (n name)
      (ast-find-child
       (lambda (i d)
         (string=? (ast-child 'name d) name))
       (ast-child 'Def* n))))))
 
 (compile-ag-specifications spec))