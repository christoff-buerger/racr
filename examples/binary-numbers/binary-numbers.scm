; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Specification of the binary to decimal number attribute grammar given in
;
;                 "Semantics of Context-Free Languages"
;                           Donald E. Knuth,
;   Theory of Computing Systems, volume 2, number 2, pages 127-145,
;                            Springer, 1968

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(with-specification
 spec
 
 ;;; AST Scheme:
 
 (ast-rule 'N->L)       ; number:
 (ast-rule 'Ni:N->)     ;  integer
 (ast-rule 'Nr:N->L<L2) ;  rational
 
 (ast-rule 'L->)        ; digit list:
 (ast-rule 'Ll:L->B)    ;  leaf
 (ast-rule 'Ln:L->L-B)  ;  node
 
 (ast-rule 'B->)        ; binary digit:
 (ast-rule 'Bz:B->)     ;  0
 (ast-rule 'Bo:B->)     ;  1
 
 (compile-ast-specifications 'N)
 
 ;;; Attribution:
 
 (ag-rule
  v ; synthesised attribute
  (Bz      (lambda (n) 0))
  (Bo      (lambda (n) (expt 2 (att-value 's n))))
  (Ll      (lambda (n) (att-value 'v (ast-child 1 n))))
  (Ln      (lambda (n) (+ (att-value 'v (ast-child 1 n))
                          (att-value 'v (ast-child 2 n)))))
  (Ni      (lambda (n) (att-value 'v (ast-child 1 n))))
  (Nr      (lambda (n) (+ (att-value 'v (ast-child 1 n))
                          (att-value 'v (ast-child 2 n))))))
 
 (ag-rule
  l ; synthesised attribute
  (Ll      (lambda (n) 1))
  (Ln      (lambda (n) (+ (att-value 'l (ast-child 1 n)) 1))))
 
 (ag-rule
  s ; inherited attribute
  ((Ll B)  (lambda (n) (att-value 's (ast-parent n))))
  ((Ln L)  (lambda (n) (+ (att-value 's (ast-parent n)) 1)))
  ((Ln B)  (lambda (n) (att-value 's (ast-parent n))))
  ((Ni L)  (lambda (n) 0))
  ((Nr L)  (lambda (n) 0))
  ((Nr L2) (lambda (n) (- (att-value 'l n)))))
 
 (compile-ag-specifications))

;;; Syntax:

(define (parse in)
  (define pos 0)
  (define (exception)
    (raise-continuable (condition (make-message-condition "Syntax Error"))))
  (define (p-char) ; #f, when no further character
    (and (< pos (string-length in)) (string-ref in pos)))
  (define (r-char) ; exception, when no further character
    (let ((c (p-char))) (unless c (exception)) (set! pos (+ pos 1)) c))
  (define (parse-digit)
    (define c (r-char))
    (create-ast
     spec
     (cond ((char=? c #\0) 'Bz) ((char=? c #\1) 'Bo) (else (exception)))
     (list)))
  (define (parse-digit-list)
    (let loop ((L (create-ast spec 'Ll (list (parse-digit)))))
      (if (let ((c (p-char))) (or (not c) (char=? c #\.)))
          L (loop (create-ast spec 'Ln (list L (parse-digit)))))))
  (define first (parse-digit-list))
  (define second? (and (p-char) (r-char) (parse-digit-list)))
  (when (p-char) (exception))
  (if second?
      (create-ast spec 'Nr (list first second?))
      (create-ast spec 'Ni (list first))))

; Given an input string encoding a binary number, return the number's decimal
; value. If the string does not encode a binary number, an exception is thrown.
(define (bin->dec input)
  (att-value 'v (parse input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Examples and Tests                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
  ; Test parser:
  (parse "0")
  (parse "1")
  (parse "010011")
  (parse "101100")
  (parse "0.0")
  (parse "0.101100")
  (parse "0.010011")
  (parse "001101.101100")
  (parse "110010.010011")
  
  (assert-exception (parse ""))
  (assert-exception (parse "."))
  (assert-exception (parse ".1"))
  (assert-exception (parse "1."))
  (assert-exception (parse "1a"))
  (assert-exception (parse "a1"))
  (assert-exception (parse "a.1"))
  (assert-exception (parse "1.a"))
  (assert-exception (parse "1a.1"))
  (assert-exception (parse "a1.1"))
  (assert-exception (parse "1.1a"))
  (assert-exception (parse "1.a1"))
  
  ; Test interpreter:
  (assert (= (bin->dec "0") 0))
  (assert (= (bin->dec "1") 1))
  (assert (= (bin->dec "01") 1))
  (assert (= (bin->dec "10") 2))
  (assert (= (bin->dec "101") 5))
  (assert (= (bin->dec "0.0") 0))
  (assert (= (bin->dec "0.1") 1/2))
  (assert (= (bin->dec "0.10") 1/2))
  (assert (= (bin->dec "0.01") 1/4))
  (assert (= (bin->dec "0.101") 5/8))
  (assert (= (bin->dec "1.1") 3/2))
  (assert (= (bin->dec "10.01") 9/4))
  (assert (= (bin->dec "101.101") 45/8))
  (assert (= (bin->dec "101.111") 47/8))
  (assert (= (bin->dec "111.111") 63/8))
  (assert (= (bin->dec "111.101") 61/8)))

(run-tests)