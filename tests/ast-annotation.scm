; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr-meta core) (racr-meta testing))

(define (debug-specification spec)
  (print-ast
   (racr-specification-2-ast-scheme spec)
   (list
    (cons 'local-correct?
          (lambda (v) (and v #t)))
    (cons 'derivable
          (lambda (v) (map (lambda (n) (ast-child 'name n)) v))))
   (current-output-port)))

(define annotation-operations
  (list
   (lambda (n) (not (ast-annotation? n 'annotation)))
   (lambda (n) (ast-annotation-set! n 'annotation #t))
   (lambda (n) (ast-annotation? n 'annotation))
   (lambda (n) (ast-annotation n 'annotation))
   (lambda (n) (ast-annotation-remove! n 'annotation))
   (lambda (n) (not (ast-annotation? n 'annotation)))))

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

(define (run-error-cases)
  (define spec (create-test-language))
  (let ((D (create-ast-2 spec 'D (list))))
    (for-each ; Intra-AST annotations throughout attribute evaluation:
     (lambda (op) (assert-exception (att-value 'intra-ast D op #t)))
     annotation-operations)
    (for-each ; Mutual-dependent inter-AST annotations throughout attribute evaluation:
     (lambda (op) (assert-exception (att-value 'mutual-dependent-inter-ast D op #t)))
     annotation-operations))
  )

(define (run-correct-cases)
  (define spec (create-test-language))
  #| TODO: enable tests when attribution meta-facilities (cf. issue #63) are implemented.
  (let ((D (create-ast-2 spec 'D (list))))
    (assert ; Intra-AST annotations outside attribute evaluation:
     (for-all (lambda (op) ((att-value 'intra-ast D op #f))) annotation-operations))
    (assert ; Independent inter-AST annotations outside attribute evaluation:
     (for-all (lambda (op) ((att-value 'independent-inter-ast D op #f))) annotation-operations))
    (assert ; Mutual-dependent inter-AST annotations outside attribute evaluation:
     (for-all (lambda (op) ((att-value 'mutual-dependent-inter-ast D op #f))) annotation-operations))
    (assert ; Independent inter-AST annotations throughout attribute evaluation:
     (for-all (lambda (op) (att-value 'independent-inter-ast D op #t)) annotation-operations)))
  |#
  #t)

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(run-tests)
(create-test-ast)
