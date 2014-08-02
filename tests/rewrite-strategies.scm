; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define run-tests
  (lambda ()
    (let ((spec (create-specification)))
      (with-specification
       spec
       (ast-rule 'S->A)
       (ast-rule 'A->B)
       (ast-rule 'Aa:A->)
       (ast-rule 'B->)
       (ast-rule 'Ba:B->)
       (compile-ast-specifications 'S)
       (compile-ag-specifications))
      
      (let (; Construct two identical test ASTs of the form
            ;    S
            ;    |
            ;    A
            ;    |
            ;    B
            (ast1 (create-ast spec 'S (list (create-ast spec 'A (list (create-ast spec 'B (list)))))))
            (ast2 (create-ast spec 'S (list (create-ast spec 'A (list (create-ast spec 'B (list)))))))
            (transformers
             (list
              (lambda (n) ; Refine A if B is not refined
                (and (eq? (ast-node-type n) 'A) (eq? (ast-node-type (ast-child 'B n)) 'B) (rewrite-refine n 'Aa)))
              (lambda (n) ; Refine B
                (and (eq? (ast-node-type n) 'B) (rewrite-refine n 'Ba))))))
        
        (apply perform-rewrites ast1 'top-down transformers) ; First the A node is refined, afterwards the B node
        (assert (eq? (ast-node-type (ast-child 'A ast1)) 'Aa))
        (assert (eq? (ast-node-type (ast-child 'B (ast-child 'A ast1))) 'Ba))
        
        (apply perform-rewrites ast2 'bottom-up transformers) ; First the B node is refined for which reason the A node never is
        (assert (eq? (ast-node-type (ast-child 'A ast2)) 'A))
        (assert (eq? (ast-node-type (ast-child 'B (ast-child 'A ast2))) 'Ba))))
    
    (let ((ast
           (with-specification
            (create-specification)
            (ast-rule 'S->A)
            (ast-rule 'A->)
            (ast-rule 'Aa:A->B)
            (ast-rule 'B->)
            (compile-ast-specifications 'S)
            (compile-ag-specifications)
            (create-ast 'S (list (create-ast 'Aa (list (create-ast 'B (list)))))))))
      (assert ; Assert, that...
       (perform-rewrites ; ...the following rewrites terminate, which...
        ast
        'top-down ; ...is only the case for proper top-down rewriting!
        (lambda (n) ; Transformer always applicable as long as there exist B nodes within the AST to transform
          (and (ast-subtype? n 'B) (rewrite-refine n 'B)))
        (lambda (n) ; Transformer eliminating B nodes
          (and (ast-subtype? n 'Aa) (rewrite-abstract n 'A))))))))

(run-tests)