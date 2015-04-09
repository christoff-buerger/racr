; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Specification of the cookie automaton given on page 21, Figure 2.1 in
; 
;               "Petrinetze: Modellierungstechnik, Analysemethoden, Fallstudien"
;                                     Wolfgang Reisig
;                                  Vieweg+Teubner, 2010
;                                    978-3-8348-1290-2

#!r6rs

(import (rnrs) (rnrs mutable-pairs) (racr core) (racr testing)
        (atomic-petrinets main)
        (atomic-petrinets user-interface)
        (atomic-petrinets execution-semantics))

(define (make-cookie-automaton)
  (petrinet:
   ((H 'Box 'Box 'Box 'Box 'Box 'Box* 'Box*)
    (D 'Token)
    (G 'Token)
    (E 7)
    (A) (B) (C) (F))
   
   (transition:
    c
    ((D (token (eq? token 'Token))))
    ((A 'Euro)))
   
   (transition:
    e
    ((A (euro (eq? euro 'Euro))))
    ((D 'Token)))
   
   (transition:
    a
    ((A (euro (eq? euro 'Euro)))
     (E (x (>= x 2)))
     (G (token (eq? token 'Token))))
    ((D 'Token)
     (E (- x 2))
     (F 'Euro)
     (B 'Token)))
   
   (transition:
    b
    ((B (token (eq? token 'Token)))
     (H (y #t) (z #t)))
    ((G 'Token)
     (C y z)))
   
   (transition:
    d
    ((C (y #t)))
    ())))

;(define-syntax assert-marking-syntax
;  (syntax-rules ()
;    ((_ net (place token-value ...) ...)
;     (assert-marking net (list 'place token-value ...) ...))))

(define (assert-marking net . marking) ; marking: list of place name followed by its tokens.
  (define marked (map (lambda (m) (=p-lookup net (car m))) marking))
  (define marked-marking (map cdr marking))
  (define !marked (filter (lambda (n) (not (memq n marked))) (->* (->Place* net))))
  (define !marked-marking (map (lambda (n) (list)) !marked))
  (define (check-place place expected-tokens)
    (define given-values (map ->value (->* (->Token* place))))
    (define-record-type nil-record (sealed #t)(opaque #t))
    (define Ok (make-nil-record))
    (for-each
     (lambda (expected-token)
       (let ((value-found? (member expected-token given-values)))
         (assert value-found?)
         (set-car! value-found? Ok)))
     expected-tokens)
    (assert (for-all nil-record? given-values)))
  (for-each check-place marked marked-marking)
  (for-each check-place !marked !marked-marking))

(define (assert-enabled net . enabled)
  (define t-enabled (map (lambda (t) (=t-lookup net t)) enabled))
  (define t-!enabled (filter (lambda (t) (not (memq t t-enabled))) (->* (->Transition* net))))
  (assert (for-all =enabled? t-enabled))
  (assert (for-all (lambda (t) (not (=enabled? t))) t-!enabled))
  (assert-exception (for-all fire-transition! t-!enabled)))

(define (run-tests)
  (define net                 (make-cookie-automaton))
  (define (marking! . m)      (apply assert-marking net m))
  (define (enabled! . t)      (apply assert-enabled net t))
  (define (fire! t)           (fire-transition! (=t-lookup net t)))
  
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'a 'e)
  (fire! 'a)
  (enabled! 'b 'c)
  (fire! 'c)
  (enabled! 'b 'e)
  (fire! 'e)
  (enabled! 'b 'c)
  (fire! 'c)
  (enabled! 'b 'e)
  (fire! 'b)
  (enabled! 'a 'd 'e)
  (fire! 'e)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'a 'd 'e)
  (fire! 'd)
  (enabled! 'a 'e 'd)
  (fire! 'd)
  (enabled! 'a 'e)
  (fire! 'a)
  (enabled! 'b 'c)
  (fire! 'b)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'a 'd 'e)
  (fire! 'a)
  (enabled! 'b 'c 'd)
  (fire! 'b)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'e)
  (fire! 'e)
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'e)
  (fire! 'e)
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'e)
  (fire! 'e)
  ; ...
  (marking! (list 'D 'Token)
            (list 'E 1)
            (list 'F 'Euro 'Euro 'Euro)
            (list 'G 'Token)
            (list 'H 'Box*))
  
  ;(marking! 'A)
  ;(marking! 'B)
  ;(marking! 'C)
  ;(marking! 'D 'Token)
  ;(marking! 'E 1)
  ;(marking! 'F 'Euro 'Euro 'Euro)
  ;(marking! 'G 'Token)
  ;(marking! 'H 'Box*)
  )

(run-tests)
