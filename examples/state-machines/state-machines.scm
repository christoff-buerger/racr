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
(define (->name n)                (ast-child 'name n))
(define (->final n)               (ast-child 'final n))
(define (->source n)              (ast-child 'source n))
(define (->target n)              (ast-child 'target n))
(define (->* n)                   (ast-children n))

; Attribute Accessors:
(define (=transitions n)          (att-value 'transitions n))
(define (=lookup-state n s)       (att-value 'lookup-state n s))
(define (=initial-state n)        (att-value 'initial-state n))
(define (=successors n)           (att-value 'successors n))
(define (=reachable n)            (att-value 'reachable n))
(define (=correct? n)             (att-value 'correct? n))

; AST Constructors:
(define (:StateMachine s t i)
  (create-ast
   sm-spec
   'StateMachine
   (list (create-ast-list s) (create-ast-list t) i)))
(define (:State n f)
  (create-ast sm-spec 'State (list n f)))
(define (:Transition s t)
  (create-ast sm-spec 'Transition (list s t)))

; Support functions:
(define (make-set l)
  (let loop ((input l) (output (list)))
    (cond
      ((null? input)
       output)
      ((memq (car input) output)
       (loop (cdr input) output))
      (else (loop (cdr input) (cons (car input) output))))))
(define (set-union s1 s2)
  (append (filter (lambda (e1) (not (memq e1 s2))) s1) s2))
(define (set-difference s1 s2)
  (remp (lambda (e) (memq e s2)) s1))
(define (set-eq? s1 s2)
  (and (= (length s1) (length s2)) (for-all (lambda (e) (memq e s2)) s1)))

(with-specification
 sm-spec
 
 ;;; AST Scheme:
 
 (ast-rule 'StateMachine->State*-Transition*-initial)
 (ast-rule 'State->name-final)
 (ast-rule 'Transition->source-target)
 
 (compile-ast-specifications 'StateMachine)
 
 ;;; Query Support:
 
 (ag-rule
  transitions ; List of transitions of state machine.
  (StateMachine
   (lambda (n)
     (->* (->Transition* n)))))
 
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
 
 ;;; Reachability:
 
 (ag-rule
  successors ; Compute a state's successor states.
  (State
   (lambda (n)
     (define name (->name n))
     (map
      (lambda (n) (=lookup-state n (->target n)))
      (filter
       (lambda (n) (eq? (->source n) name))
       (=transitions n))))))
 
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
  
  (StateMachine ; The state machine is correct, if...
   (lambda (n)
     (and
      (=initial-state n) ; ...its inital state is defined and...
      (not ; ...all its states and...
       (ast-find-child
        (lambda (i n) (not (=correct? n)))
        (->State* n)))
      (not ; ...transitions are correct.
       (ast-find-child
        (lambda (i n) (not (=correct? n)))
        (->Transition* n))))))

  (Transition ; A transition is correct, if...
   (lambda (n)
     (define source (->source n))
     (define target (->target n))
     (and
      (symbol? source) ; ...its source and...
      (symbol? target) ; ...target are symbols...
      (=lookup-state n source) ; ...refering to respectively named states.
      (=lookup-state n target))))
  
  (State ; A state is correct, if...
   (lambda (n)
     (define name (->name n))
     (and
      (symbol? name) ; ...its name is a symbol and...
      (eq? (=lookup-state n name) n) ; ...unique and...
      (or ; ...from every state, except finals, a final is reachable and...
       (->final n)
       (find ->final (=reachable n)))
      (or ; ...all states, except the initial, are reachable from the initial.
       (eq? n (=initial-state n))
       (memq n (=reachable (=initial-state n))))))))
 
 (compile-ag-specifications))

;;; Syntax:

(define-syntax state-machine:
  (syntax-rules (->)
    ((_ initial (final ...) (source -> target) ...)
     (let* ((f (make-set (list 'final ...)))
            (s (set-difference (make-set (list 'source ... 'target ...)) f)))
       (:StateMachine
        (append
         (map (lambda (s) (:State s #t)) f)
         (map (lambda (s) (:State s #f)) s))
        (list (:Transition 'source 'target) ...)
        'initial)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Examples and tests                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
  (define (assert-sucessors sm s l)
    (assert (set-eq? (map ->name (=successors (=lookup-state sm s))) l)))
  (define (assert-reachable sm s l)
    (assert (set-eq? (map ->name (=reachable (=lookup-state sm s))) l)))
  
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
  (rewrite-add (->State* sm-1) (:State 'sn #f))
  (rewrite-add (->Transition* sm-1) (:Transition 's3 'sn))
  (assert (not (=correct? sm-1)))
  (rewrite-terminal 'final (=lookup-state sm-1 'sn) #t)
  (assert (=correct? sm-1))
  (rewrite-terminal 'final (=lookup-state sm-1 'sn) #f)
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