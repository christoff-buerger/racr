; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; The following rewrite tests verify, that only attributes influenced by rewrites are flushed. For that purpose we maintain for
; every attribute instance an evaluation-counter which states how often the instance has been evaluated. The value of instances
; is the value of their respective evaluation counter. To simulate dependencies to other nodes and attribute instances, attribute
; equations contain respective AST accesses. Given such a setting, incremental attribute evaluation can be tested by
; simply comparing the value of attribute instances to their expected number of evaluations.

#!r6rs

(import (rnrs) (racr))

(define run-tests
  (lambda ()
    (let* (;;; Test compiler:
           (state-table (make-hashtable equal-hash equal? 50))
           (next-state
            (lambda (att n)
              (let ((result (+ (hashtable-ref state-table (cons att n) 0) 1)))
                (hashtable-set! state-table (cons att n) result)
                result)))
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
               (lambda (n)
                 (att-value 's (ast-child 1 n))
                 (next-state 's n)))
              (A
               (lambda (n)
                 (ast-find-child
                  (lambda (i e)
                    (cdr (att-value 's e)))
                  (ast-child 1 n))
                 (next-state 's n)))
              (E
               (lambda (n)
                 (cons (next-state 's n) (ast-child 1 n)))))
             
             (ag-rule
              i
              ((S A)
               (lambda (n)
                 (att-value 's n)
                 (next-state 'i n)))
              ((A E*)
               (lambda (n)
                 (att-value 'i (ast-parent n))
                 (next-state 'i n))))
             
             (ag-rule
              A-length
              (A
               (lambda (n)
                 (ast-num-children (ast-child 1 n))
                 (next-state 'A-length n)))
              (S
               (lambda (n)
                 (att-value 'A-length (ast-child 1 n))
                 (next-state 'A-length n))))
             
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
            (lambda (lb . ub)
              (if (null? ub)
                  (ast-child lb (A-E-list))
                  (ast-children (A-E-list) (cons lb (car ub))))))
           
           ;;; Test infrastructure:
           (reference-state-table (make-hashtable equal-hash equal? 50))
           (influenced: ; Function that given a list of attribute contexts checks, that only the given contexts are influenced by the last rewrite:
            (lambda args
              (define evaluation-checker
                (lambda (flushed? n att . args)
                  (cond
                    ((list? n)
                     (for-each
                      (lambda (e)
                        (evaluation-checker flushed? e att))
                      n))
                    (else
                     (hashtable-update!
                      reference-state-table
                      (cons att n)
                      (lambda (state)
                        (let ((value (att-value att n))
                              (state (if flushed? (+ state 1) state)))
                          (assert (= (if (pair? value) (car value) value) state))
                          state))
                      0)))
                  (if (not (null? args))
                      (apply evaluation-checker flushed? args))))
              ; Ensure, that the specified influenced attributes have been flushed and reevaluated:
              (if (not (null? args))
                  (apply evaluation-checker #t args))
              ; Ensure, that no further attributes have been flushed and reevaluated:
              (apply
               evaluation-checker
               #f
               (append
                (list
                 (S) 's
                 (A) 's
                 (A) 'i
                 (S) 'A-length
                 (A) 'A-length)
                (if (not (null? (ast-children (A-E-list))))
                    (list
                     (E 1 (ast-num-children (A-E-list))) 's
                     (E 1 (ast-num-children (A-E-list))) 'i)
                    (list)))))))
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
        (E 3) 's
        (E 4 5) 's) ; Fix!
       
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
        (A) 'A-length
        (E 4 5) 'i ; Fix!
        (E 4 5) 's) ; Fix!
       
       ; delete before hit:
       (rewrite-delete (E 2))
       (influenced:
        (S) 's
        (A) 's
        (A) 'i
        (S) 'A-length
        (A) 'A-length
        (E 1 4) 'i
        (E 2 4) 's) ; Fix!
       
       ; insert after hit:
       (rewrite-insert (A-E-list) 3 (create-ast 'E (list #f)))
       (influenced:
        (S) 'A-length
        (A) 'A-length
        (E 3) 'i
        (E 3) 's
        (E 4 5) 'i ; Fix!
        (E 4 5) 's) ; Fix!
       
       ; insert before hit:
       (rewrite-insert (A-E-list) 1 (create-ast 'E (list #f)))
       (influenced:
        (S) 's
        (A) 's
        (A) 'i
        (S) 'A-length
        (A) 'A-length
        (E 1 6) 'i
        (E 1) 's
        (E 2 6) 's) ; Fix!
       
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
        (E 1 6) 'i
        (E 4 6) 's) ; Fix!
       
       ; insert without hit:
       (rewrite-insert (A-E-list) 2 (create-ast 'E (list #f)))
       (influenced:
        (S) 's
        (A) 's
        (A) 'i
        (S) 'A-length
        (A) 'A-length
        (E 1 7) 'i
        (E 2 2) 's
        (E 3 7) 's))))) ; Fix!