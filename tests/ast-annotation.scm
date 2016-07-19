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
