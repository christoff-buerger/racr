; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr-meta core) (racr-meta ast-annotation) (racr-meta testing))

(define annotation-operations
  (let ((v (lambda x x)))
    (list
     (lambda (n) (undefined-annotation? (ast-annotation n 'permanent)))
     (lambda (n) (ast-annotation-set! n 'permanent v) #t) ; Execute following operations on annotated node.
     (lambda (n) (undefined-annotation? (ast-annotation n #t)))
     (lambda (n) (undefined-annotation? (ast-annotation-remove! n #t)))
     (lambda (n) (undefined-annotation? (ast-annotation-remove! n 'undefined)))
     (lambda (n) (undefined-annotation? (ast-annotation n 'annotation)))
     (lambda (n) (ast-annotation-set! n 'annotation v) #t)
     (lambda (n)
       (define value (ast-annotation n 'annotation))
       (and (not (undefined-annotation? value)) (eq? value v)))
     (lambda (n) (ast-annotation-set! n 'annotation 1) #t)
     (lambda (n)
       (define value (ast-annotation n 'annotation))
       (and (not (undefined-annotation? value)) (= value 1)))
     (lambda (n)
       (define value (ast-annotation-remove! n 'annotation))
       (and (not (undefined-annotation? value)) (= value 1)))
     (lambda (n) (undefined-annotation? (ast-annotation n 'annotation)))
     (lambda (n) (ast-annotation-set! n 'annotation (ast-annotation n 'permanent)) #t)
     (lambda (n)
       (define value (ast-annotation-remove! n 'annotation))
       (and (not (undefined-annotation? value)) (eq? value v)))
     (lambda (n) (undefined-annotation? (ast-annotation n 'annotation)))
     (lambda (n)
       (define value (ast-annotation n 'permanent))
       (and (not (undefined-annotation? value)) (eq? value v)))
     (lambda (n)
       (define value (ast-annotation-remove! n 'permanent))
       (and (not (undefined-annotation? value)) (eq? value v)))
     (lambda (n) (undefined-annotation? (ast-annotation n 'permanent))))))

(define (create-test-language)
  (define spec (create-specification-2))
  ;;; AST scheme:
  (ast-rule-2 spec 'A->t1-D)
  (ast-rule-2 spec 'B:A->A-t2)
  (ast-rule-2 spec 'C:A->A*-t2)
  (ast-rule-2 spec 'D->)
  (specify-start-symbol-2 spec 'A)
  ;;; Attribution:
  (specify-attribute-2 ; Inter-AST reference.
   spec
   'n
   'D
   '*
   #t
   (lambda (n)
     (create-ast-2 spec 'D (list)))
   #f)
  ; Depending on attribute arguments, either annotate AST of the attribute (intra-AST annotation)
  ; or return a function doing so:
  (specify-attribute-2
   spec
   'intra-ast
   'D
   '*
   #t
   (lambda (n annotation-operation apply-throughout-evaluation?)
     (define (f) (annotation-operation n))
     (or (and apply-throughout-evaluation? (f)) f))
   #f)
  ; Depending on attribute arguments, either annotate foreign AST (independent inter-AST
  ; annotation) or return a function doing so:
  (specify-attribute-2
   spec
   'independent-inter-ast
   'D
   '*
   #t
   (lambda (n annotation-operation apply-throughout-evaluation?)
     (define (f) (annotation-operation (att-value 'n n)))
     (or (and apply-throughout-evaluation? (f)) f))
   #f)
  ; Annotate foreign AST with function performing intra-AST annotation (mutual-dependent inter-AST
  ; annotation). Depending on attribute arguments, either execute the function or return it:
  (specify-attribute-2
   spec
   'mutual-dependent-inter-ast
   'D
   '*
   #t
   (lambda (n annotation-operation apply-throughout-evaluation?)
     (define (f) ((ast-annotation (att-value 'n n) 'f)))
     (ast-annotation-set! (att-value 'n n) 'f (lambda () (annotation-operation n)))
     (or (and apply-throughout-evaluation? (f)) f))
   #f)
  ;;; Compile specification and return it:
  (compile-specification-2 spec)
  spec)

; Construct AST containing all combinations of child/parent contexts (including bud nodes):
;
;  B14
;  / \
; _  C13  
;    / \
;  D12  *
;     / | \
;   A9 C10 B11
;  /  / |  | \
; D6 _  *  D7 B8
;     / | \   | \
;   C2  _  B3 D4 A5
;  / |     |\     |
; D1 _     _ _    _
(define (create-test-ast)
  (define spec (create-test-language))
  (define D1 (create-ast-2 spec 'D (list)))
  (define C2 (create-ast-2 spec 'C (list #t D1 (create-ast-bud) #t)))
  (define B3 (create-ast-2 spec 'B (list #t (create-ast-bud) (create-ast-bud) #t)))
  (define D4 (create-ast-2 spec 'D (list)))
  (define A5 (create-ast-2 spec 'A (list #t (create-ast-bud))))
  (define D6 (create-ast-2 spec 'D (list)))
  (define D7 (create-ast-2 spec 'D (list)))
  (define B8 (create-ast-2 spec 'B (list #t D4 A5 #t)))
  (define A9 (create-ast-2 spec 'A (list #t D6)))
  (define C10
    (create-ast-2
     spec
     'C
     (list #t (create-ast-bud) (create-ast-list-2 (list C2 (create-ast-bud) B3)) #t)))
  (define B11 (create-ast-2 spec 'B (list #t D7 B8 #t)))
  (define D12 (create-ast-2 spec 'D (list)))
  (define C13 (create-ast-2 spec 'C (list #t D12 (create-ast-list-2 (list A9 C10 B11)) #t)))
  (define B14 (create-ast-2 spec 'B (list #t (create-ast-bud) C13 #t)))
  B14)

(define (C13 n) (ast-child 'A n))
(define (D12 n) (ast-child 'D (C13 n)))
(define (A9 n)  (ast-child 1 (ast-child 'A* (C13 n))))
(define (C10 n) (ast-child 2 (ast-child 'A* (C13 n))))
(define (B11 n) (ast-child 3 (ast-child 'A* (C13 n))))
(define (D6 n)  (ast-child 'D (A9 n)))
(define (D7 n)  (ast-child 'D (B11 n)))
(define (B8 n)  (ast-child 'A (B11 n)))
(define (C2 n)  (ast-child 1 (ast-child 'A* (C10 n))))
(define (B3 n)  (ast-child 3 (ast-child 'A* (C10 n))))
(define (D4 n)  (ast-child 'D (B8 n)))
(define (A5 n)  (ast-child 'A (B8 n)))
(define (D1 n)  (ast-child 'D (C2 n)))

(define (run-error-cases)
  (define spec (create-test-language))
  
  (let ((D (create-ast-2 spec 'D (list)))) ; Basic error cases:
    (for-each (lambda (op) (assert-exception (op #t))) annotation-operations) ; Context is not an AST node.
    (assert-exception (ast-annotation-set! D #t #t)) ; Invalid annotation name.
    (assert-exception (ast-annotation-set! D 'prohibited (ast-annotation D 'undefined)))) ; Prohibited value.
  
  (let ((D (create-ast-2 spec 'D (list)))) ; Annotations and attribute evaluation:
    (for-each ; Intra-AST annotations throughout attribute evaluation.
     (lambda (op) (assert-exception (att-value 'intra-ast D op #t)))
     annotation-operations)
    (for-each ; Mutual-dependent inter-AST annotations throughout attribute evaluation.
     (lambda (op) (assert-exception (att-value 'mutual-dependent-inter-ast D op #t)))
     annotation-operations)))

(define (run-correct-cases)
  (define spec (create-test-language))
  
  (let ((D (create-ast-2 spec 'D (list)))) ; Basic valid cases:
    (assert (for-all (lambda (op) (op D)) annotation-operations)))
  
  #| TODO: enable tests when attribution meta-facilities (cf. issue #63) are implemented.
  (let ((D (create-ast-2 spec 'D (list)))) ; Annotations and attribute evaluation:
    (assert ; Independent inter-AST annotations throughout attribute evaluation.
     (for-all (lambda (op) (att-value 'independent-inter-ast D op #t)) annotation-operations))
    (assert ; Intra-AST annotations outside attribute evaluation.
     (for-all (lambda (op) ((att-value 'intra-ast D op #f))) annotation-operations))
    (assert ; Independent inter-AST annotations outside attribute evaluation.
     (for-all (lambda (op) ((att-value 'independent-inter-ast D op #f))) annotation-operations))
    (assert ; Mutual-dependent inter-AST annotations outside attribute evaluation.
     (for-all (lambda (op) ((att-value 'mutual-dependent-inter-ast D op #f))) annotation-operations)))
  |#
  )

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(run-tests)