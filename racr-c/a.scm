;#!r6rs
;(import (rnrs) (racr))


(let*
 ((state 0)
  (node
   (with-specification
    (create-specification)
    (ast-rule 'A->)
    (compile-ast-specifications 'A)
    (ag-rule state
     (A #f
      (lambda (n)
       (set! state (+ state 1))
       state)))
    (compile-ag-specifications)
    (create-ast 'A (list)))))
 (display (att-value 'state node)) (newline)
 (display (att-value 'state node)) (newline)
 (display (att-value 'state node)) (newline)
 node)

