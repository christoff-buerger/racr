; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define test-language (create-specification))

(define run-error-cases
  (lambda ()
    (with-specification
     test-language
     (assert-exception (create-ast 'UNKNOWN (list))) ; Unknown non-terminal
     (assert-exception (create-ast 'A (list))) ; Insufficient many candidates
     (assert-exception (create-ast 'A (list (create-ast-bud) 1))) ; To many candidates
     (assert-exception (create-ast 'A (list (create-ast 'A (list (create-ast-bud)))))) ; Unexpected non-list candidate
     (assert-exception (create-ast 'A (list (create-ast-list (list))))) ; Unexpected list candidate
     (assert-exception ; List does not fit because of element of wrong type
      (create-ast
       'C
       (list
        (create-ast-bud)
        (create-ast-bud)
        (create-ast-list
         (list))
        'terminal
        (create-ast-bud)
        (create-ast-list
         (list (create-ast 'A (list (create-ast-bud))))))))
     (assert-exception (create-ast-list (list 'terminal))) ; Terminal as list element
     (assert-exception (create-ast-list (list (create-ast-list (list))))) ; Nested lists
     (let ((ast (create-ast 'A (list (create-ast-bud))))) ; Candidate already part of AST
       (assert-exception (create-ast 'A (list (ast-child 'D ast))))
       (assert-exception (create-ast-list (list (ast-child 'D ast)))))
     (assert-exception (att-value 'erroneous-attribute (create-ast 'D (list))))))) ; Candidate in evaluation

(define run-correct-cases
  (lambda ()
    #f))

(define run-tests
  (lambda ()
    (run-error-cases)
    (run-correct-cases)))

(when (= (specification->phase test-language) 1)
  (with-specification
   test-language
   (ast-rule 'A->D)
   (ast-rule 'B:A->)
   (ast-rule 'C:A->A-A*-t-B-B*)
   (ast-rule 'D->)
   (compile-ast-specifications 'A)
   (ag-rule
    erroneous-attribute
    (D
     (lambda (n)
       (create-ast 'A (list n)))))
   (compile-ag-specifications)))

(run-tests)