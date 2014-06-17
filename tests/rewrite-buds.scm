; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

; Given an AST node, return a list of the buds within its spaned tree.
(define collect-all-buds
  (lambda (n)
    (if (ast-bud-node? n)
        (list n)
        (fold-left
         (lambda (result child)
           (if (ast-node? child)
               (append result (collect-all-buds child))
               result))
         (list)
         (ast-children n)))))

; Given an AST node n and arbitrary many further AST nodes l ensure, that the buds of the AST n spans are l.
(define assert-buds
  (lambda (n . expected-buds)
    (let ((actual-buds (collect-all-buds n)))
      (assert (= (length actual-buds) (length expected-buds)))
      (assert
       (null?
        (filter
         (lambda (bud)
           (not (memq bud expected-buds)))
         actual-buds))))))

(define run-tests
  (lambda ()
    (let* ((spec (create-specification))
           (ast
            (with-specification
             spec
             (ast-rule 'S->A)
             (ast-rule 'A->B-B*)
             (ast-rule 'B->)
             (compile-ast-specifications 'S)
             (compile-ag-specifications)
             (create-ast 'S (list (create-ast-bud))))))
      (with-specification
       spec
       
       (assert-buds
        ast
        (ast-child 'A ast))
       
       (rewrite-subtree ; Replace bud by node
        (ast-child 'A ast)
        (create-ast
         'A
         (list
          (create-ast 'B (list))
          (create-ast-list
           (list
            (create-ast 'B (list))
            (create-ast-bud)
            (create-ast 'B (list)))))))
       (assert-buds
        ast
        (ast-child 2 (ast-child 'B* (ast-child 'A ast))))
       
       (let ((fragment-to-move1 (ast-child 'B (ast-child 'A ast)))
             (fragment-to-move2 (ast-child 2 (ast-child 'B* (ast-child 'A ast)))))
         (rewrite-subtree ; Replace node by bud
          fragment-to-move1
          (create-ast-bud))
         (rewrite-subtree ; Replace bud by node
          fragment-to-move2
          fragment-to-move1)
         (rewrite-add ; Add bud to list
          (ast-parent fragment-to-move1)
          fragment-to-move2))
       (assert-buds
        ast
        (ast-child 'B (ast-child 'A ast))
        (ast-child 4 (ast-child 'B* (ast-child 'A ast))))
       
       (rewrite-insert ; Insert bud into list
        (ast-child 'B* (ast-child 'A ast))
        1
        (create-ast-bud))
       (assert-buds
        ast
        (ast-child 'B (ast-child 'A ast))
        (ast-child 1 (ast-child 'B* (ast-child 'A ast)))
        (ast-child 5 (ast-child 'B* (ast-child 'A ast))))
       
       (rewrite-delete ; Delete bud from list
        (ast-child 5 (ast-child 'B* (ast-child 'A ast))))
       (assert-buds
        ast
        (ast-child 'B (ast-child 'A ast))
        (ast-child 1 (ast-child 'B* (ast-child 'A ast))))))))

(run-tests)