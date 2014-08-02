; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define ast-spec
  (lambda ()
    (let ((spec (create-specification)))
      (with-specification
       spec
       (ast-rule 'A->ref)
       (ast-rule 'B:A->A<A1-A*<A2)
       (ast-rule 'C:B->Z-A<A3)
       (ast-rule 'Z->ref)
       (compile-ast-specifications 'A)
       spec))))

(define filter-matches
  (lambda (n pattern-name type)
    (filter
     (lambda (n)
       (att-value pattern-name n))
     (collect-nodes-of-subtype n type))))

(define collect-nodes-of-subtype
  (lambda (n type)
    (cond
      ((ast-bud-node? n)
       (list))
      ((ast-list-node? n)
       (fold-left
        (lambda (result n)
          (append result (collect-nodes-of-subtype n type)))
        (list)
        (ast-children n)))
      (else
       (let ((children
              (fold-left
               (lambda (result non-terminal? child)
                 (if non-terminal?
                     (append result (collect-nodes-of-subtype child type))
                     result))
               (list)
               (map
                symbol->non-terminal?
                (cdr
                 (ast-rule->production
                  (specification->find-ast-rule
                   (ast-specification n)
                   (ast-node-type n)))))
               (ast-children n))))
         (if (ast-subtype? n type)
             (cons n children)
             children))))))

(define erroneous-pattern-tests
  (lambda ()
    ;;; Distinguished node and redeclaration errors:
    
    (assert-exception
     (specify-pattern ; ERROR: Undefined distinguished node
      (ast-spec)
      'pattern
      'D
      '((A
         #f
         ()))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Distinguished node has no type
      (ast-spec)
      'pattern
      'D
      '((#f
         D
         ()))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Distinguished node is list-node
      (ast-spec)
      'pattern
      'D
      '((*
         D
         ()))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Bounded name redeclaration
      (ast-spec)
      'pattern
      'D
      '((*
         J
         ((1 A D ())
          (2 A J ()))))
      '()
      #f))
    
    ;;; Reachability errors:
    
    (assert-exception
     (specify-pattern
      (ast-spec)
      'pattern
      'D
      '((A
         D
         ())
        (#f
         F1
         ())
        (#f
         F2
         ())
        (#f
         F3
         ())
        (#f ; Not reachable
         F4
         ()))
      '((ref D F1)
        (ref F1 F2)
        (ref F2 F3)
        (ref F4 F2))
      #f))
    
    ;;; Type errors:
    
    (assert-exception
     (specify-pattern ; ERROR: Undefined node type
      (ast-spec)
      'pattern
      'D
      '((B
         D
         ((A1 Undefined #f ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: User defined nested lists
      (ast-spec)
      'pattern
      'D
      '((A
         D
         ())
        (*
         R
         ((1 * #f ()))))
      '((ref D R))
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Context defined nested lists
      (ast-spec)
      'pattern
      'D
      '((A
         D
         ())
        (#f
         R
         ((1
           #f
           #f
           ((1 #f #f ()))))))
      '((ref D R))
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Mixed list/non-list node
      (ast-spec)
      'pattern
      'D
      '((#f ; Cannot be list and non-list at the same time
         #f
         ((1 A D ())
          (A1 #f #f ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Undefined context
      (ast-spec)
      'pattern
      'D
      '((B
         D
         ((Undefined #f #f ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: No unique non-list contexts
      (ast-spec)
      'pattern
      'D
      '((A
         D
         ((A1 #f #f ())
          (A1 #f #f ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: No unique list contexts
      (ast-spec)
      'pattern
      'D
      '((#f
         #f
         ((1 A D ())
          (1 #f #f ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Unsatisfyable context type
      (ast-spec)
      'pattern
      'D
      '((C
         D
         ((A3
           Z ; Type = A (A3 child of type C is of type A)
           #f
           ()))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Unsatisfyable pattern
      (ast-spec)
      'pattern
      'D
      '((A
         D
         ((A2
           #f ; Type = * (A2 child of type A is a list)
           #f
           ((1
             #f ; Type = C (only type C has Z child)
             #f
             ((Z
               A ; Type = Z (Z child of type C is of type Z)
               #f
               ()))))))))
      '()
      #f))
    
    (assert-exception
     (specify-pattern ; ERROR: Unsatisfyable pattern
      (ast-spec)
      'pattern
      'D
      '((* ; Type Z never is subject of a Kleene closure
         #f
         ((1 Z D ()))))
      '()
      #f))))

(define correct-pattern-tests
  (lambda ()
    (let ((ast
           (with-specification
            (ast-spec)
            
            (ag-rule
             ref
             (A
              (lambda (n)
                (ast-child 'ref n)))
             (Z
              (lambda (n)
                (ast-child 'ref n))))
            
            (specify-pattern
             'pattern1
             'D
             '((A D ()))
             '()
             #f)
            
            (specify-pattern
             'pattern2
             'D
             '((B
                D
                ((A1 C #f ())
                 (A2
                  *
                  #f
                  ((1 A #f ())
                   (3 #f #f ()))))))
             '()
             #f)
            
            (specify-pattern
             'pattern3
             'D
             '((C
                D
                ((A3 #f R1s ())))
               (C
                R2s
                ((A2
                  #f
                  #f
                  ((3 #f R1t ())))))
               (A R2t ()))
             '((ref R1s R1t)
               (ref R2s R2t))
             #f)
            
            (specify-pattern
             'pattern4
             'D
             '((C
                D
                ((A3 #f R1s ())))
               (C
                R2s
                ((A2
                  #f
                  #f
                  ((3 #f R1t ()))))))
             '((ref R1s R1t)
               (ref R2s D))
             #f)
            
            (specify-pattern
             'pattern5
             'D
             '((A D ()))
             '()
             (with-bindings
              (D)
              (ast-child 'ref D)))
            
            (compile-ag-specifications)
            
            (create-ast
             'B ; pattern1
             (list
              #f
              (create-ast
               'C ; pattern1, pattern2
               (list
                #f
                (create-ast
                 'C ; pattern1, pattern2 (pattern3, pattern4: Only with proper references)
                 (list
                  #f
                  (create-ast
                   'C ; pattern1
                   (list
                    #f
                    (create-ast-bud)
                    (create-ast-list (list))
                    (create-ast-bud)
                    (create-ast-bud)))
                  (create-ast-list
                   (list
                    (create-ast 'A (list #f)) ; pattern1
                    (create-ast-bud)
                    (create-ast-bud)))
                  (create-ast-bud)
                  (create-ast 'A (list #f)))) ; pattern1
                (create-ast-list
                 (list
                  (create-ast 'A (list #f)) ; pattern1
                  (create-ast-bud)
                  (create-ast 'A (list #f)) ; pattern1
                  (create-ast-bud)))
                (create-ast-bud)
                (create-ast-bud)))
              (create-ast-list (list)))))))
      (assert (= (length (filter-matches ast 'pattern1 'A)) 8))
      (assert (= (length (att-value 'pattern1 ast)) 1))
      (assert (eq? (cdr (assq 'D (att-value 'pattern1 ast))) ast))
      (assert (= (length (filter-matches ast 'pattern2 'B)) 2))
      (assert (null? (filter-matches ast 'pattern3 'C)))
      (assert (null? (filter-matches ast 'pattern4 'C)))
      (assert (null? (filter-matches ast 'pattern5 'A)))
      
      (rewrite-terminal
       1
       (ast-child 'A3 (ast-child 'A1 (ast-child 'A1 ast)))
       (ast-child 3 (ast-child 'A2 (ast-child 'A1 ast))))
      (rewrite-terminal
       1
       (ast-child 'A1 ast)
       (ast-child 1 (ast-child 'A2 (ast-child 'A1 ast))))
      (assert (= (length (filter-matches ast 'pattern1 'A)) 8))
      (assert (= (length (att-value 'pattern1 ast)) 1))
      (assert (eq? (cdr (assq 'D (att-value 'pattern1 ast))) ast))
      (assert (= (length (filter-matches ast 'pattern2 'B)) 2))
      (assert (= (length (filter-matches ast 'pattern3 'C)) 1))
      (assert
       (eq?
        (cdr (assq 'R2t (att-value 'pattern3 (car (filter-matches ast 'pattern3 'C)))))
        (ast-child 1 (ast-child 'A2 (ast-child 'A1 ast)))))
      (assert (null? (filter-matches ast 'pattern4 'C)))
      (assert (= (length (filter-matches ast 'pattern5 'A)) 2))
      
      (rewrite-terminal
       1
       (ast-child 'A1 ast)
       (ast-child 'A1 (ast-child 'A1 ast)))
      (assert (= (length (filter-matches ast 'pattern1 'A)) 8))
      (assert (= (length (att-value 'pattern1 ast)) 1))
      (assert (eq? (cdr (assq 'D (att-value 'pattern1 ast))) ast))
      (assert (= (length (filter-matches ast 'pattern2 'B)) 2))
      (assert (= (length (filter-matches ast 'pattern3 'C)) 1))
      (assert
       (eq?
        (cdr (assq 'R2t (att-value 'pattern3 (car (filter-matches ast 'pattern3 'C)))))
        (ast-child 'A1 (ast-child 'A1 ast))))
      (assert (= (length (filter-matches ast 'pattern4 'C)) 1))
      (assert
       (eq?
        (cdr (assq 'R2t (att-value 'pattern3 (car (filter-matches ast 'pattern4 'C)))))
        (ast-child 'A1 (ast-child 'A1 ast))))
      (assert (= (length (filter-matches ast 'pattern5 'A)) 2)))))

(define run-tests
  (lambda ()
    (erroneous-pattern-tests)
    (correct-pattern-tests)))

(run-tests)