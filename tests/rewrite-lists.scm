; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define (basic-tests)
  (let-values (((rt-equation influenced:) (construct-reevaluation-tests)))
    (let* ((spec (create-specification))
           ; Test language:
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
                
             ; Test AST with following attribute dependencies:
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
              
           ; AST access functions:
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
        (E 2 2) 's)))))

(define (re-entrant-tests) ; issue #77
  (define (index* p c)
    (ast-find-child*
     (lambda (i child)
       (if (eq? child c) i #f))
     p))

  (define (name* p i)
    (ast-child 'name (ast-child i p)))

  (define spec
    (let ((spec (create-specification)))
      (with-specification
       spec

       (ast-rule 'A->B*)
       (ast-rule 'B->name)
       (compile-ast-specifications 'A)

       ; Re-entrant list-children dependency tests:
       (ag-rule index-f  (B #f (lambda (n)   (index* (ast-parent n) n))))
       (ag-rule index-t  (B    (lambda (n)   (index* (ast-parent n) n))))
       (ag-rule index-t* (B    (lambda (n)   (att-value 'index* (ast-parent n) n))))
       (ag-rule index*   (A    (lambda (n c) (index* (ast-child 'B* n) c))))
       (ag-rule name1    (B    (lambda (n)   (name* (ast-parent n) 1))))
       (ag-rule name2    (B    (lambda (n)   (name* (ast-parent n) 2))))
       (ag-rule name3    (B    (lambda (n)   (name* (ast-parent n) 3))))
       (ag-rule name4    (B    (lambda (n)   (name* (ast-parent n) 4))))
       (ag-rule name5    (B    (lambda (n)   (name* (ast-parent n) 5))))
       (ag-rule name1*   (B    (lambda (n)   (att-value 'name* (ast-parent n) 1))))
       (ag-rule name2*   (B    (lambda (n)   (att-value 'name* (ast-parent n) 2))))
       (ag-rule name3*   (B    (lambda (n)   (att-value 'name* (ast-parent n) 3))))
       (ag-rule name4*   (B    (lambda (n)   (att-value 'name* (ast-parent n) 4))))
       (ag-rule name5*   (B    (lambda (n)   (att-value 'name* (ast-parent n) 5))))
       (ag-rule name*    (A    (lambda (n i) (name* (ast-child 'B* n) i))))
       (compile-ag-specifications))

      spec))
  
  (define (A . b)
    (create-ast spec 'A (list (create-ast-list b))))

  (define (B name)
    (create-ast spec 'B (list name)))

  (define (assert-attributes n)
    (define names-count (ast-num-children (ast-child 'B* n)))
    (ast-for-each-child
     (lambda (i n)
       (assert (eq? (att-value 'index-t* n) (att-value 'index-t n)))
       (assert (eq? (att-value 'index-t* n) (att-value 'index-f n)))
       (let loop ((i names-count))
         (when (> i 0)
           (let* ((name  (string->symbol (string-append "name" (number->string i))))
                  (name* (string->symbol (string-append "name" (number->string i) "*"))))
             (assert (eq? (att-value name n) (att-value name* n))))
           (loop (- i 1)))))
     (ast-child 'B* n)))

  (define ast (A (B 'a) (B 'b) (B 'c) (B 'd)))

  (do ((i 1 (+ i 1))) ((> i 5))
    (assert-attributes ast)
    (rewrite-insert (ast-child 'B* ast) i (B 'inserted))
    (assert-attributes ast)
    (rewrite-delete (ast-child i (ast-child 'B* ast)))
    (assert-attributes ast)))

(define (run-tests)
  (basic-tests)
  (re-entrant-tests))

(run-tests)