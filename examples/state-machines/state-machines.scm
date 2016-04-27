; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core))

(define sm-spec                   (create-specification))

; AST Accessors:
(define (->State* n)              (ast-child 'State* n))
(define (->Transition* n)         (ast-child 'Transition* n))
(define (->initial n)             (ast-child 'initial n))
(define (->finals n)              (ast-child 'finals n))
(define (->name n)                (ast-child 'name n))
(define (->source n)              (ast-child 'source n))
(define (->target n)              (ast-child 'target n))
(define (->* n)                   (ast-children n))
(define (<- n)                    (ast-parent n))

; Attribute Accessors:
(define (=final? n)               (att-value 'final? n))
(define (=lookup-state n s)       (att-value 'lookup-state n s))
(define (=initial-state n)        (att-value 'initial-state n))
(define (=filter-transitions n f) (att-value 'filter-transitions n f))
(define (=successors n)           (att-value 'successors n))
(define (=reachable n)            (att-value 'reachable n))
(define (=correct? n)             (att-value 'correct? n))

; AST Constructors:
(define (:StateMachine s t i f)
  (create-ast sm-spec 'StateMachine
              (list (create-ast-list s) (create-ast-list t) i f)))
(define (:State n)
  (create-ast sm-spec 'State (list n)))
(define (:Transition s t)
  (create-ast sm-spec 'Transition (list s t)))

; Support functions:
(define (set-union s1 s2)
  (append (filter (lambda (e1) (not (memq e1 s2))) s1) s2))
(define (set-eq s1 s2)
  (and (= (length s1) (length s2)) (for-all (lambda (e) (memq e s2)) s1)))

(with-specification
 sm-spec
 
 ;;; AST Scheme:
 
 (ast-rule 'StateMachine->State*-Transition*-initial-finals)
 (ast-rule 'State->name)
 (ast-rule 'Transition->source-target)
 
 (compile-ast-specifications 'StateMachine)
 
 ;;; Query Support:
 
 (ag-rule
  final? ; Return, whether a state is a final state or not.
  ((StateMachine State*)
   (lambda (n)
     (define name (->name n))
     (find
      (lambda (final) (eq? name final))
      (->finals (<- (<- n)))))))
 
 ;;; Name Analysis:
 
 (ag-rule
  lookup-state ; Find a certain state by its name.
  (StateMachine
   (lambda (n name)
     (ast-find-child
      (lambda (i n) (eq? (->name n) name))
      (->State* n)))))
 
 (ag-rule
  initial-state ; Return the state machine's initial state.
  (StateMachine
   (lambda (n)
     (=lookup-state n (->initial n)))))
 
 (ag-rule
  filter-transitions ; Return all transitions satisfying a given filter.
  (StateMachine
   #f
   (lambda (n f)
     (filter f (->* (->Transition* n))))))
 
 ;;; Reachability:
 
 (ag-rule
  successors ; Compute a state's successor states.
  (State
   (lambda (n)
     (define name (->name n))
     (map
      (lambda (n) (=lookup-state n (->target n)))
      (=filter-transitions n (lambda (n) (eq? (->source n) name)))))))
 
 (ag-rule
  reachable ; Compute all states reachable from a state.
  (State
   (lambda (n)
     (fold-left
      (lambda (result n) (set-union result (=reachable n)))
      (=successors n)
      (=successors n)))
   (list)
   (lambda (r1 r2)
     (= (length r1) (length r2)))))
 
 ;;; Well-formedness Analysis:
 
 (ag-rule
  correct? ; Check well-formedness semantics.
  
  (StateMachine ; The state machine is correct, iff all states are correct.
   (lambda (n)
     (not
      (ast-find-child
       (lambda (i n) (not (=correct? n)))
       (->State* n)))))
  
  (State
   (lambda (n)
     (and
      (or ; From every state, except finals, a final must be reachable.
       (=final? n)
       (find =final? (=reachable n)))
      (or ; All states, except the initial, must be reachable from the initial.
       (eq? n (=initial-state n))
       (memq n (=reachable (=initial-state n))))))))
 
 (compile-ag-specifications))

;;; Syntax:

(define-syntax state-machine:
  (lambda (x)
    (define identifier-list?
      (lambda (l)
        (for-all identifier? l)))
    (syntax-case x (->)
      ((_ initial (final ...) (source -> target) ...)
       (and
        (identifier? #'initial)
        (identifier-list? #'(final ...))
        (identifier-list? #'(source ...))
        (identifier-list? #'(target ...))
        (not (null? #'(final ...))))
       (with-syntax
           (((state ...)
             (let loop ((input #'(initial final ... source ... target ...))
                        (output (list)))
               (if (null? input)
                   output
                   (if (find
                        (lambda (o)
                          (eq? (syntax->datum (car input)) (syntax->datum o)))
                        output)
                       (loop (cdr input) output)
                       (loop (cdr input) (cons (car input) output)))))))
         #`(:StateMachine
            (list (:State 'state) ...)
            (list (:Transition 'source 'target) ...)
            'initial
            (list 'final ...)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Examples and tests                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
  (define (assert-sucessors sm s l)
    (assert (set-eq (map ->name (=successors (=lookup-state sm s))) l)))
  (define (assert-reachable sm s l)
    (assert (set-eq (map ->name (=reachable (=lookup-state sm s))) l)))
  
  (define sm-1
    ; Correct machine:
    ;        /--\
    ;        |  |
    ;  /---*s1--/
    ; /    /  \
    ; |   s2   s3
    ; |  /  \  |
    ; \-s4* s5 s6
    ;   | \ |  |
    ;   |  \s7 |
    ;   \      /
    ;    \----/
    (state-machine:
     s1
     (s4)
     (s1 -> s2)
     (s1 -> s3)
     (s1 -> s1)
     (s2 -> s4)
     (s2 -> s5)
     (s3 -> s6)
     (s6 -> s4)
     (s5 -> s7)
     (s7 -> s4)
     (s4 -> s1)))
  
  (define sm-2
    ; Incorrect machine (s4 not reachable from the initial state s1):
    ; *s1 -- s2 -- s3*
    ;       /
    ;  s4 -/ 
    (state-machine:
     s1
     (s3)
     (s1 -> s2)
     (s2 -> s3)
     (s4 -> s2)))
  
  (define sm-3
    ; Incorrect machine (s2 and s4 cannot reach the final state s5):
    ; *s1
    ;  /  \
    ; s2  s3
    ; |    |
    ; s4  s5*
    (state-machine:
     s1
     (s5)
     (s1 -> s2)
     (s1 -> s3)
     (s2 -> s4)
     (s3 -> s5)))
  
  (define sm-4
    ; Correct state machine:
    ;    *s1---s9---s10*
    ;   /   \
    ; s2     s3
    ; |     /  \
    ; s4---s5  s6
    ;  \       /
    ;  s7*---s8
    (state-machine:
     s1
     (s7 s10)
     (s1 -> s2)
     (s1 -> s3)
     (s1 -> s9)
     (s9 -> s10)
     (s2 -> s4)
     (s3 -> s5)
     (s3 -> s6)
     (s5 -> s4)
     (s4 -> s7)
     (s6 -> s8)
     (s8 -> s7)))
  
  ; Test sm-1:
  (assert (=lookup-state sm-1 's5))
  (assert (not (=lookup-state sm-1 'unknown-state)))
  (assert-sucessors sm-1 's1 '(s1 s2 s3))
  (assert-reachable sm-1 's6 '(s1 s2 s3 s4 s5 s6 s7))
  (assert (=correct? sm-1))
  (rewrite-add (->State* sm-1) (:State 'sn))
  (rewrite-add (->Transition* sm-1) (:Transition 's3 'sn))
  (assert (not (=correct? sm-1)))
  (rewrite-terminal 'finals sm-1 (cons 'sn (->finals sm-1)))
  (assert (=correct? sm-1))
  (rewrite-terminal 'finals sm-1 (list 's4))
  (assert (not (=correct? sm-1)))
  (rewrite-add (->Transition* sm-1) (:Transition 'sn 's5))
  (assert (=correct? sm-1))
  
  ; Test sm-2:
  (assert (not (=correct? sm-2)))
  
  ; Test sm-3:
  (assert (not (=correct? sm-3)))
  
  ; Test sm-4:
  (assert (=correct? sm-4))
  (assert-reachable sm-4 's3 '(s5 s4 s7 s6 s8))
  (assert-reachable sm-4 's6 '(s8 s7))
  (assert-reachable sm-4 's5 '(s4 s7))
  (assert-reachable sm-4 's2 '(s4 s7)))

(run-tests)