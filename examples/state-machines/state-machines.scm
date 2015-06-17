; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs
(import (rnrs) (racr core))

(define sm-spec (create-specification))

(with-specification
  sm-spec
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                     Abstract Syntax Tree Specification                   ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (ast-rule 'StateMachine->State*-Transition*-initial-finals)
  (ast-rule 'State->name)
  (ast-rule 'Transition->source-target)
  
  (compile-ast-specifications 'StateMachine)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                      Attribute Grammar Specification                     ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;; Support
  (ag-rule
   final? ; Return, whether a state is a final state or not.
   ((StateMachine State*)
    (lambda (n)
      (let ((name (ast-child 'name n)))
        (find
         (lambda (final)
           (eq? name final))
         (ast-child 'finals (ast-parent (ast-parent n))))))))
  
  ;;; Name analysis
  (ag-rule
   lookup-state ; Find a certain state by its name.
   (StateMachine
    (lambda (n name)
      (ast-find-child
       (lambda (i state)
         (eq? (ast-child 'name state) name))
       (ast-child 'State* n)))))
  
  (ag-rule
   initial-state ; Return the state machine's initial state.
   (StateMachine
    (lambda (n)
      (att-value 'lookup-state n (ast-child 'initial n)))))
  
  (ag-rule
   filter-transitions ; Return all transitions satisfying a given filter.
   (StateMachine
    #f
    (lambda (n filter-function)
      (filter
       filter-function
       (ast-children (ast-child 'Transition* n))))))
  
  ;;; Reachability
  (ag-rule
   successors ; Compute a state's successor states.
   (State
    (lambda (n)
      (map
       (lambda (trans)
         (att-value 'lookup-state n (ast-child 'target trans)))
       (att-value
        'filter-transitions
        n
        (lambda (trans)
          (eq? (ast-child 'source trans) (ast-child 'name n))))))))
  
  (ag-rule
   reachable ; Compute all states reachable from a state.
   (State
    (lambda (n)
      (let ((successors (att-value 'successors n)))
        (fold-left
         (lambda (result state)
           (list-union result (att-value 'reachable state)))
         successors
         successors)))
    (list)
    (lambda (r1 r2)
      (= (length r1) (length r2)))))
  
  ;;; Constraints
  (ag-rule
   correct? ; Check well-formedness semantics.
   (StateMachine ; The state machine is correct, iff all states are correct.
    (lambda (n)
      (not
       (ast-find-child
        (lambda (i state)
          (not (att-value 'correct? state)))
        (ast-child 'State* n)))))
   (State
    (lambda (n)
      (and
       (or ; From every state, except final states, a final state must be reachable.
        (att-value 'final? n)
        (find
         (lambda (reachable-state)
           (att-value 'final? reachable-state))
         (att-value 'reachable n)))
       (let ((initial-state (att-value 'initial-state n))) ; Every state, except the initial state, must be reachable from the initial state.
         (or
          (eq? n initial-state)
          (find
           (lambda (state)
             (eq? n state))
           (att-value 'reachable initial-state))))))))
  
  (compile-ag-specifications))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             Support Functions                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-union
  (lambda (l1 l2)
    (append
     (filter
      (lambda (e1)
        (not (memq e1 l2)))
      l1)
     l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               User Interface                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax state-machine
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
       (with-syntax (((state ...)
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
         #`(create-ast
            sm-spec
            'StateMachine
            (list
             (create-ast-list (list (create-ast sm-spec 'State (list 'state)) ...))
             (create-ast-list (list (create-ast sm-spec 'Transition (list 'source 'target)) ...))
             'initial
             (list 'final ...))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Examples and Tests                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define run-tests
  (lambda ()
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
      (state-machine
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
      (state-machine
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
      (state-machine
       s1
       (s5)
       (s1 -> s2)
       (s1 -> s3)
       (s2 -> s4)
       (s3 -> s5)))
    (define sm-4
      ; Correct state machine:
      ;          *s1---s9---s10*
      ;         /   \
      ;       s2     s3
      ;       |     /  \
      ;       s4---s5  s6
      ;        \       /
      ;         s7*---s8
      (state-machine
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
    (assert (att-value 'lookup-state sm-1 's5))
    (assert (not (att-value 'lookup-state sm-1 'unknown-state)))
    (assert
     (let ((successors (att-value 'successors (att-value 'lookup-state sm-1 's1))))
      (and
       (= (length successors) 3)
       (memq (att-value 'lookup-state sm-1 's1) successors)
       (memq (att-value 'lookup-state sm-1 's2) successors)
       (memq (att-value 'lookup-state sm-1 's3) successors))))
    (assert
     (let ((reachable (att-value 'reachable (att-value 'lookup-state sm-1 's6))))
       (and
        (= (length reachable) 7)
        (memq (att-value 'lookup-state sm-1 's1) reachable)
        (memq (att-value 'lookup-state sm-1 's2) reachable)
        (memq (att-value 'lookup-state sm-1 's3) reachable)
        (memq (att-value 'lookup-state sm-1 's4) reachable)
        (memq (att-value 'lookup-state sm-1 's5) reachable)
        (memq (att-value 'lookup-state sm-1 's6) reachable)
        (memq (att-value 'lookup-state sm-1 's7) reachable))))
    (assert (att-value 'correct? sm-1))
    (rewrite-add (ast-child 'State* sm-1) (create-ast sm-spec 'State (list 'sn)))
    (rewrite-add (ast-child 'Transition* sm-1) (create-ast sm-spec 'Transition (list 's3 'sn)))
    (assert (not (att-value 'correct? sm-1)))
    (rewrite-terminal 'finals sm-1 (cons 'sn (ast-child 'finals sm-1)))
    (assert (att-value 'correct? sm-1))
    (rewrite-terminal 'finals sm-1 (list 's4))
    (assert (not (att-value 'correct? sm-1)))
    (rewrite-add (ast-child 'Transition* sm-1) (create-ast sm-spec 'Transition (list 'sn 's5)))
    (assert (att-value 'correct? sm-1))
    
    ; Test sm-2:
    (assert (not (att-value 'correct? sm-2)))
    
    ; Test sm-3:
    (assert (not (att-value 'correct? sm-3)))
    
    ; Test sm-4:
    (assert (att-value 'correct? sm-4))
    (assert
     (let ((reachable (att-value 'reachable (att-value 'lookup-state sm-4 's3))))
       (and
        (= (length reachable) 5)
        (memq (att-value 'lookup-state sm-4 's5) reachable)
        (memq (att-value 'lookup-state sm-4 's4) reachable)
        (memq (att-value 'lookup-state sm-4 's7) reachable)
        (memq (att-value 'lookup-state sm-4 's6) reachable)
        (memq (att-value 'lookup-state sm-4 's8) reachable))))
    (assert
     (let ((reachable (att-value 'reachable (att-value 'lookup-state sm-4 's6))))
       (and
        (= (length reachable) 2)
        (memq (att-value 'lookup-state sm-4 's8) reachable)
        (memq (att-value 'lookup-state sm-4 's7) reachable))))
    (assert
     (let ((reachable (att-value 'reachable (att-value 'lookup-state sm-4 's5))))
       (and
        (= (length reachable) 2)
        (memq (att-value 'lookup-state sm-4 's4) reachable)
        (memq (att-value 'lookup-state sm-4 's7) reachable))))
    (assert
     (let ((reachable (att-value 'reachable (att-value 'lookup-state sm-4 's2))))
       (and
        (= (length reachable) 2)
        (memq (att-value 'lookup-state sm-4 's4) reachable)
        (memq (att-value 'lookup-state sm-4 's7) reachable))))))

(run-tests)