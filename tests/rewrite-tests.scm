; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Unit tests for RACR's rewrite interface. The purpose of the rewrite tests is to verify, that only
; attributes influenced by rewrites are flushed. For that purpose we maintain for every attribute-location
; an evaluation-counter which states how often the attribute-location has been evaluated (i.e., how often
; the equations of equally named attributes at the respective location have been applied / how often the
; equation of a certain attribute instance at the respective location has been applied). The value
; computed by the application of an attribute's equation just is its evaluation-counter. Dependencies to other
; nodes and attributes are achieved as usual by calling the respective node or attribute. Given such a setting,
; rewrites and required attribute dependency and cache maintainance can be easily tested by simply comparing
; every attribute's value to the expected number of evaluations after performing a rewrite.

#!r6rs

(import (rnrs) (racr))

(define init-basic-test
  (lambda ()
    (let* ((state-table (make-hashtable equal-hash equal? 50))
           (next-state
            (lambda (att n)
              (let ((result (+ (hashtable-ref state-table (cons att n) 0) 1)))
                (hashtable-set! state-table (cons att n) result)
                result)))
           (spec (create-specification)))
      (with-specification
       spec
       
       ;;; Specify simple test language:
       (ast-rule 'S->A)
       (ast-rule 'A->B-C)
       (ast-rule 'B->)
       (ast-rule 'Ba:B->terminal)
       (ast-rule 'C->D)
       (ast-rule 'D->terminal)
       
       (compile-ast-specifications 'S)
       
       (ag-rule
        att
        (S
         0
         (lambda (n)
           (att-value 'att (ast-child 1 n))
           (next-state 'att 'S)))
        (A
         0
         (lambda (n)
           (att-value 'att (ast-child 1 n))
           (next-state 'att 'A)))
        (A
         1
         (lambda (n)
           (att-value 'att (ast-sibling 2 n))
           (next-state 'att 'B)))
        (Ba
         0
         (lambda (n)
           (ast-child 1 n)
           (next-state 'att 'B)))
        (A
         2
         (lambda (n)
           (att-value 'att (ast-child 1 n))
           (next-state 'att 'C)))
        (D
         0
         (lambda (n)
           (ast-child 1 n)
           (next-state 'att 'D))))
       
       (ag-rule
        att2
        (A
         2
         (lambda (n)
           (att-value 'att (ast-sibling 1 n))
           (next-state 'att2 'C)))
        (C
         1
         (lambda (n)
           (att-value 'att2 (ast-parent n))
           (next-state 'att2 'D))))
       
       (compile-ag-specifications)
       
       ;;; Return RACR specification and test AST:
       (values
        spec
        ; Attribute dependencies (* = att; # = att2):
        ;      S *
        ;        |
        ;      A *
        ;       /
        ;      //----\
        ;    B *--* C #
        ;         | | |
        ;         * D #
        (create-ast
         'S
         (list
          (create-ast
           'A
           (list
            (create-ast 'B (list))
            (create-ast
             'C
             (list
              (create-ast 'D (list 1)))))))))))))

(define init-list-test
  (lambda ()
    (let* ((state-table (make-hashtable equal-hash equal? 50))
           (next-state
            (lambda (att n)
              (let ((result (+ (hashtable-ref state-table (cons att n) 0) 1)))
                (hashtable-set! state-table (cons att n) result)
                result)))
           (spec (create-specification)))
      (with-specification
       spec
       
       ;;; Specify simple test language:
       (ast-rule 'S->A)
       (ast-rule 'A->E*)
       (ast-rule 'E->t)
       
       (compile-ast-specifications 'S)
       
       (ag-rule
        s
        (S
         0
         (lambda (n)
           (att-value 's (ast-child 1 n))
           (next-state 's n)))
        (A
         0
         (lambda (n)
           (ast-find-child
            (lambda (i e)
              (cdr (att-value 's e)))
            (ast-child 1 n))
           (next-state 's n)))
        (E
         0
         (lambda (n)
           (cons (next-state 's n) (ast-child 1 n)))))
       
       (ag-rule
        i
        (S
         1
         (lambda (n)
           (att-value 's n)
           (next-state 'i n)))
        (A
         1
         (lambda (n)
           (att-value 'i (ast-parent n))
           (next-state 'i n))))
       
       (ag-rule
        A-length
        (A
         0
         (lambda (n)
           (ast-num-children (ast-child 1 n))
           (next-state 'A-length n)))
        (S
         0
         (lambda (n)
           (att-value 'A-length (ast-child 1 n))
           (next-state 'A-length n))))
       
       (compile-ag-specifications)
       
       ;;; Return RACR specification and test AST:
       (values
        spec
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
               (list #f)))))))))))))

(define run-tests
  (lambda ()
    ;;; Basic tests:
    (let*-values (((spec ast) (init-basic-test))
                  ((S) ast)
                  ((A) (lambda() (ast-child 1 S)))
                  ((B) (lambda() (ast-child 1 (A))))
                  ((C) (lambda() (ast-child 2 (A))))
                  ((D) (lambda() (ast-child 1 (C)))))
      (with-specification
       spec
       
       (assert (= (att-value 'att S) 1))
       (assert (= (att-value 'att (A)) 1))
       (assert (= (att-value 'att (B)) 1))
       (assert (= (att-value 'att (C)) 1))
       (assert (= (att-value 'att (D)) 1))
       (assert (= (att-value 'att2 (C)) 1))
       (assert (= (att-value 'att2 (D)) 1))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 1))
       (assert (= (att-value 'att (A)) 1))
       (assert (= (att-value 'att (B)) 1))
       (assert (= (att-value 'att (C)) 1))
       (assert (= (att-value 'att (D)) 1))
       (assert (= (att-value 'att2 (C)) 1))
       (assert (= (att-value 'att2 (D)) 1))
       
       (rewrite-terminal 1 (D) 10) ; Influences: D.att, C.att, B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       
       (rewrite-terminal 1 (D) (ast-child 1 (D))) ; No change at all => no attribute caches are flushed
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       
       (rewrite-node (B) (create-ast 'Ba (list -10))) ; Influences: B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       
       (rewrite-terminal 1 (D) 20) ; Influences: D.att, C.att
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       
       (rewrite-node (B) (create-ast 'B (list))) ; Influences: B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 4))
       (assert (= (att-value 'att (A)) 4))
       (assert (= (att-value 'att (B)) 4))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 4))
       (assert (= (att-value 'att2 (D)) 4))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 4))
       (assert (= (att-value 'att (A)) 4))
       (assert (= (att-value 'att (B)) 4))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 4))
       (assert (= (att-value 'att2 (D)) 4))
       
       (rewrite-terminal 1 (D) 1) ; Influences: D.att, C.att, B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 5))
       (assert (= (att-value 'att (A)) 5))
       (assert (= (att-value 'att (B)) 5))
       (assert (= (att-value 'att (C)) 4))
       (assert (= (att-value 'att (D)) 4))
       (assert (= (att-value 'att2 (C)) 5))
       (assert (= (att-value 'att2 (D)) 5))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 5))
       (assert (= (att-value 'att (A)) 5))
       (assert (= (att-value 'att (B)) 5))
       (assert (= (att-value 'att (C)) 4))
       (assert (= (att-value 'att (D)) 4))
       (assert (= (att-value 'att2 (C)) 5))
       (assert (= (att-value 'att2 (D)) 5))))
    
    ;;; List tests:
    (let*-values (((spec ast) (init-list-test))
                  ; AST access functions:
                  ((S)
                   (lambda ()
                     ast))
                  ((A)
                   (lambda ()
                     (ast-child 1 (S))))
                  ((A-E-list)
                   (lambda ()
                     (ast-child 1 (A))))
                  ((E)
                   (lambda (lb . ub)
                     (if (null? ub)
                         (ast-child lb (A-E-list))
                         (ast-children (A-E-list) (cons lb (car ub))))))
                  ; Evaluation state reference table:
                  ((state-table)
                   (make-hashtable equal-hash equal? 50))
                  ; Support function that given a list of attribute contexts checks, that only the given contexts
                  ; are influenced by the last rewrite:
                  ((influenced:)
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
                             state-table
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
                       (if (not (null? (ast-num-children (A-E-list))))
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
        (E 3 7) 's) ; Fix!
       ))))