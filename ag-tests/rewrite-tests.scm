; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Unit tests for RACR's rewrite interface. The purpose of the rewrite tests is to verify, that only
; attributes influenced by rewrites are flushed. For that purpose we maintain for every attribute-location
; an evaluation-counter which states how often the attribute-location has been evaluated (i.e., how often
; the equations of equally named attributes at the respective location have been applied). The value
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
         (lambda (n) (att-value 'att (ast-child 1 n)) (next-state 'att 'S)))
        (A
         0
         (lambda (n) (att-value 'att (ast-child 1 n)) (next-state 'att 'A)))
        (A
         1
         (lambda (n) (att-value 'att (ast-sibling 2 n)) (next-state 'att 'B)))
        (Ba
         0
         (lambda (n) (ast-child 1 n) (next-state 'att 'B)))
        (A
         2
         (lambda (n) (att-value 'att (ast-child 1 n)) (next-state 'att 'C)))
        (D
         0
         (lambda (n) (ast-child 1 n) (next-state 'att 'D))))
       
       (ag-rule
        att2
        (A
         2
         (lambda (n) (att-value 'att (ast-sibling 1 n)) (next-state 'att2 'C)))
        (C
         1
         (lambda (n) (att-value 'att2 (ast-parent n)) (next-state 'att2 'D))))
       
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
        ;   / \-------------                            |
        ;  /       \        \                           |
        ;  i E s   i E s ... i E s   i E s  ...  i E s  |
        ;   #f |    #f  |     #t |       |           |  |
        ;      \        \        \       \           \  |
        ;       \---------------------------------------/
        (create-ast
         'S
         (list
          (create-ast
           'A
           (list
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
              (list #f))))))))))))

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
                  ((S) ast)
                  ((A) (lambda () (ast-child 1 S)))
                  ((A-E-list) (lambda () (ast-child 1 (A))))
                  ((E) (lambda (i) (ast-child i (A-E-list))))
                  ((assert-E-list)
                   (lambda (i-values s-values)
                     (assert (= (length (ast-children (ast-child 1 (A)))) (length i-values)))
                     (assert (= (length s-values) (length i-values)))
                     (let loop ((E-list (ast-children (ast-child 1 (A))))
                                (i-values i-values)
                                (s-values s-values))
                       (if (not (null? E-list))
                           (begin
                             (assert (= (att-value 'i (car E-list)) (car i-values)))
                             (assert (= (car (att-value 's (car E-list))) (car s-values)))
                             (loop (cdr E-list) (cdr i-values) (cdr s-values)))))))
                  ((state-table) (make-hashtable equal-hash equal? 50))
                  ((E2)
                   (lambda (lb ub)
                     (ast-children (A-E-list) (cons lb ub)))))
      (with-specification
       spec
       
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
               (apply evaluation-checker (cons flushed? args)))))
       
       (define influenced:
         (lambda args
           (apply evaluation-checker (cons #t args))))
       
       (define not-influenced:
         (lambda args
           (apply evaluation-checker (cons #f args))))
       
       (define ensure-fixpoint
         (lambda ()
           (not-influenced:
            S 's
            (A) 's
            S 'A-length
            (A) 'A-length
            (E2 1 (ast-num-children (A-E-list))) 's
            (E2 1 (ast-num-children (A-E-list))) 'i)))
       
       (influenced:
        S 's
        (A) 's
        S 'A-length
        (A) 'A-length
        (E2 1 4) 'i
        (E2 1 4) 's)
       (ensure-fixpoint)
       
       (assert (= (att-value 's S) 1))
       (assert (= (att-value 's (A)) 1))
       (assert (= (att-value 'i (A)) 1))
       (assert-E-list (list 1 1 1 1) (list 1 1 1 1))
       (assert (= (att-value 'A-length S) 1))
       (assert (= (att-value 'A-length (A)) 1))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 1))
       (assert (= (att-value 's (A)) 1))
       (assert (= (att-value 'i (A)) 1))
       (assert-E-list (list 1 1 1 1) (list 1 1 1 1))
       (assert (= (att-value 'A-length S) 1))
       (assert (= (att-value 'A-length (A)) 1))
       
       ; insert hit:
       (rewrite-insert (A-E-list) 3 (create-ast 'E (list #t))) ; Influences: A.s, S.s, A.i, E(1-4)-old.i, A.A-length, S.A-length
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 2 2) (list 1 1 1 2 2)) ; TODO: Fix E(3-4)-old.s dependencies
       (assert (= (att-value 'A-length S) 2))
       (assert (= (att-value 'A-length (A)) 2))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 2 2) (list 1 1 1 2 2)) ; TODO: Fix E(3-4)-old.s dependencies
       (assert (= (att-value 'A-length S) 2))
       (assert (= (att-value 'A-length (A)) 2))
       
       ; add with hit:
       (rewrite-add (A-E-list) (create-ast 'E (list #f))) ; Influences: A.A-length, S.A-length
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 2 2 1) (list 1 1 1 2 2 1))
       (assert (= (att-value 'A-length S) 3))
       (assert (= (att-value 'A-length (A)) 3))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 2 2 1) (list 1 1 1 2 2 1))
       (assert (= (att-value 'A-length S) 3))
       (assert (= (att-value 'A-length (A)) 3))
       
       ; delete after hit:
       (rewrite-delete (E 4)) ; Influences: A.A-length, S.A-length
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 3 2) (list 1 1 1 3 2)) ; TODO: Fix E(5-6)-old.s + E(5-6)-old.i dependencies
       (assert (= (att-value 'A-length S) 4))
       (assert (= (att-value 'A-length (A)) 4))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 2))
       (assert (= (att-value 's (A)) 2))
       (assert (= (att-value 'i (A)) 2))
       (assert-E-list (list 2 2 1 3 2) (list 1 1 1 3 2)) ; TODO: Fix E(5-6)-old.s + E(5-6)-old.i dependencies
       (assert (= (att-value 'A-length S) 4))
       (assert (= (att-value 'A-length (A)) 4))
       
       ; delete before hit:
       (rewrite-delete (E 2)) ; Influences: A.s, S.s, A.i, E(1-5)-old.i, A.A-length, S.A-length
       (assert (= (att-value 's S) 3))
       (assert (= (att-value 's (A)) 3))
       (assert (= (att-value 'i (A)) 3))
       (assert-E-list (list 3 2 4 3) (list 1 2 4 3)) ; TODO: Fix E(4-5)-old.s + E(4-5)-old.i dependencies
       (assert (= (att-value 'A-length S) 5))
       (assert (= (att-value 'A-length (A)) 5))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 3))
       (assert (= (att-value 's (A)) 3))
       (assert (= (att-value 'i (A)) 3))
       (assert-E-list (list 3 2 4 3) (list 1 2 4 3)) ; TODO: Fix E(4-5)-old.s + E(4-5)-old.i dependencies
       (assert (= (att-value 'A-length S) 5))
       (assert (= (att-value 'A-length (A)) 5))
       
       ; insert after hit:
       ; insert before hit:
       
       ; cancel hit:
       (rewrite-terminal 1 (E 2) #f) ; Influences: A.s, S.s, E(1-4).i, E(2).s
       (assert (= (att-value 's S) 4))
       (assert (= (att-value 's (A)) 4))
       (assert (= (att-value 'i (A)) 4))
       (assert-E-list (list 4 3 5 4) (list 1 3 4 3))
       (assert (= (att-value 'A-length S) 5))
       (assert (= (att-value 'A-length (A)) 5))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 4))
       (assert (= (att-value 's (A)) 4))
       (assert (= (att-value 'i (A)) 4))
       (assert-E-list (list 4 3 5 4) (list 1 3 4 3))
       (assert (= (att-value 'A-length S) 5))
       (assert (= (att-value 'A-length (A)) 5))
       
       ; add without hit:
       (rewrite-add (A-E-list) (create-ast 'E (list #f))) ; Influences: A.s, S.s, E(1-4).i, A.A-length, S.A-length
       (assert (= (att-value 's S) 5))
       (assert (= (att-value 's (A)) 5))
       (assert (= (att-value 'i (A)) 5))
       (assert-E-list (list 5 4 6 5 1) (list 1 3 4 3 1))
       (assert (= (att-value 'A-length S) 6))
       (assert (= (att-value 'A-length (A)) 6))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 5))
       (assert (= (att-value 's (A)) 5))
       (assert (= (att-value 'i (A)) 5))
       (assert-E-list (list 5 4 6 5 1) (list 1 3 4 3 1))
       (assert (= (att-value 'A-length S) 6))
       (assert (= (att-value 'A-length (A)) 6))
       
       ; delete without hit:
       
       ; insert without hit:
       (rewrite-insert (A-E-list) 2 (create-ast 'E (list #f))) ; Influences: A.s, S.s, E(1-5)-old.i, A.A-length, S.A-length
       (assert (= (att-value 's S) 6))
       (assert (= (att-value 's (A)) 6))
       (assert (= (att-value 'i (A)) 6))
       (assert-E-list (list 6 1 5 7 6 2) (list 1 1 4 5 4 2)) ; TODO: Fix E(2-5)-old.s dependency
       (assert (= (att-value 'A-length S) 7))
       (assert (= (att-value 'A-length (A)) 7))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 's S) 6))
       (assert (= (att-value 's (A)) 6))
       (assert (= (att-value 'i (A)) 6))
       (assert-E-list (list 6 1 5 7 6 2) (list 1 1 4 5 4 2)) ; TODO: Fix E(2-5)-old.s dependency
       (assert (= (att-value 'A-length S) 7))
       (assert (= (att-value 'A-length (A)) 7))))))