; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define run-tests
  (lambda ()
    (let-values (((rt-equation influenced:) (construct-reevaluation-tests)))
      (let* (;;; Test compiler:
             (spec (create-specification))
             (ast
              (with-specification
               spec
               
               (ast-rule 'S->A)
               (ast-rule 'A->E*)
               (ast-rule 'E->t)
               
               (compile-ast-specifications 'S)
               
               (ag-rule
                s
                (S
                 (rt-equation
                  's
                  (lambda (n)
                    (att-value 's (ast-child 1 n)))))
                (A
                 (rt-equation
                  's
                  (lambda (n)
                    (ast-find-child
                     (lambda (i e)
                       (att-value 's e))
                     (ast-child 1 n)))))
                (E
                 (rt-equation
                  's
                  (lambda (n)
                    (ast-child 1 n)))))
                
                (ag-rule
                 i
                 ((S A)
                  (rt-equation
                   'i
                   (lambda (n)
                     (att-value 's n))))
                 ((A E*)
                  (rt-equation
                   'i
                   (lambda (n)
                     (att-value 'i (ast-parent n))))))
                
                (ag-rule
                 A-length
                 (A
                  (rt-equation
                   'A-length
                   (lambda (n)
                     (ast-num-children (ast-child 1 n)))))
                 (S
                  (rt-equation
                   'A-length
                   (lambda (n)
                     (att-value 'A-length (ast-child 1 n))))))
                
                (compile-ag-specifications)
                
                ; Attribute dependencies:
                ;      S s
                ;        |
                ;  /----\|
                ;  \-i A s--------------------------------------\
                ;   / \--------------------------------\        |
                ;  /       \        \       \           \       |
                ;  i E s   i E s ... i E s   i E s  ...  i E s  |
                ;   #f |    #f  |     #t |                      |
                ;      \        \        \                      |
                ;       \---------------------------------------/
                (create-ast
                 'S
                 (list
                  (create-ast
                   'A
                   (list
                    (create-ast-list
                     (list
                      (create-ast
                       'E
                       (list #f))
                      (create-ast
                       'E
                       (list #f))
                      (create-ast
                       'E
                       (list #f))
                      (create-ast
                       'E
                       (list #f))))))))))
              
              ;;; Test compiler AST access functions:
              (S
               (lambda ()
                 ast))
              (A
               (lambda ()
                 (ast-child 1 (S))))
              (A-E-list
               (lambda ()
                 (ast-child 1 (A))))
              (E
               (case-lambda
                 ((lb) (ast-child lb (A-E-list)))
                 ((lb ub) (ast-children (A-E-list) (cons lb ub))))))
        (with-specification
         spec
         
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 4) 'i
          (E 1 4) 's)
         
         ; insert hit:
         (rewrite-insert (A-E-list) 3 (create-ast 'E (list #t)))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 5) 'i
          (E 3) 's)
         
         ; add with hit:
         (rewrite-add (A-E-list) (create-ast 'E (list #f)))
         (influenced:
          (S) 'A-length
          (A) 'A-length
          (E 6) 'i
          (E 6) 's)
         
         ; delete after hit:
         (rewrite-delete (E 4))
         (influenced:
          (S) 'A-length
          (A) 'A-length)
         
         ; delete before hit:
         (rewrite-delete (E 2))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 4) 'i)
         
         ; insert after hit:
         (rewrite-insert (A-E-list) 3 (create-ast 'E (list #f)))
         (influenced:
          (S) 'A-length
          (A) 'A-length
          (E 3) 'i
          (E 3) 's)
         
         ; insert before hit:
         (rewrite-insert (A-E-list) 1 (create-ast 'E (list #f)))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 6) 'i
          (E 1) 's)
         
         ; cancel hit:
         (rewrite-terminal 1 (E 3) #f)
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (E 1 6) 'i
          (E 3) 's)
         
         ; add without hit:
         (rewrite-add (A-E-list) (create-ast 'E (list #f)))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 7) 'i
          (E 7) 's)
         
         ; delete without hit:
         (rewrite-delete (E 4))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 6) 'i)
         
         ; insert without hit:
         (rewrite-insert (A-E-list) 2 (create-ast 'E (list #f)))
         (influenced:
          (S) 's
          (A) 's
          (A) 'i
          (S) 'A-length
          (A) 'A-length
          (E 1 7) 'i
          (E 2 2) 's))))))

(run-tests)