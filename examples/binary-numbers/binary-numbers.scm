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
(import (rnrs) (racr core))

(define bn-specification (create-specification))

(with-specification
 bn-specification
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                     Abstract syntax tree specification                   ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (ast-rule 'S->N) ; start separate CFG
 
 (ast-rule 'N->L)
 (ast-rule 'Ni:N->) ; integer number
 (ast-rule 'Nr:N->L<L2) ; rational number
 
 (ast-rule 'L->)
 (ast-rule 'Ll:L->B) ; numbers list leaf
 (ast-rule 'Ln:L->L-B) ; numbers list node
 
 (ast-rule 'B->)
 (ast-rule 'Bz:B->) ; digit 0
 (ast-rule 'Bo:B->) ; digit 1
 
 (compile-ast-specifications 'S)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                      Attribute grammar specification                     ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (ag-rule
  v
  (S (lambda (n) (att-value 'v (ast-child 1 n))))
  (Bz (lambda (n) 0))
  (Bo (lambda (n) (expt 2 (att-value 's n))))
  (Ll (lambda (n) (att-value 'v (ast-child 1 n))))
  (Ln (lambda (n) (+ (att-value 'v (ast-child 1 n)) (att-value 'v (ast-child 2 n)))))
  (Ni (lambda (n) (att-value 'v (ast-child 1 n))))
  (Nr (lambda (n) (+ (att-value 'v (ast-child 1 n)) (att-value 'v (ast-child 2 n))))))
 
 (ag-rule
 l
 (Ll (lambda (n) 1))
 (Ln (lambda (n) (+ (att-value 'l (ast-child 1 n)) 1))))

(ag-rule
 s
 ((Ll B) (lambda (n) (att-value 's (ast-parent n))))
 ((Ln L) (lambda (n) (+ (att-value 's (ast-parent n)) 1)))
 ((Ln B) (lambda (n) (att-value 's (ast-parent n))))
 ((Ni L) (lambda (n) 0))
 ((Nr L) (lambda (n) 0))
 ((Nr L2) (lambda (n) (- (att-value 'l n)))))

(compile-ag-specifications))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                  Parser                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an input string encoding a binary number and a continuation to use in
; case of input errors, return a function that computes the binary number's
; decimal value; Iff the input is not wellformed and the returned compilation
; function is executed, an error message is returned to the error continuation.
(define bin->dec-compiler
  (lambda (input error-continuation)
    (with-specification
     bn-specification
     (let* (;;; Support functions
           (c-length (string-length input))
           (parser-error
            (lambda ()
              (error-continuation "Syntax error; Compilation aborted!")))
           (pos 0)
           (consume-char
            (lambda ()
              (set! pos (+ pos 1))))
           (match?
            (lambda (c)
              (if (< pos c-length)
                  (char=? (string-ref input pos) c)
                  #f)))
           (match!
            (lambda (c)
              (if (match? c)
                  (consume-char)
                  (parser-error))))
           ;;; Parse functions
           (parse-B
            (lambda ()
              (if (match? #\0)
                  (begin
                    (consume-char)
                    (create-ast 'Bz (list)))
                  (begin
                    (match! #\1)
                    (create-ast 'Bo (list))))))
           (parse-L
            (lambda ()
              (let ((B-list (list)))
                (let loop ()
                  (if (or (match? #\0) (match? #\1))
                      (begin
                        (set! B-list (cons (parse-B) B-list))
                        (loop))
                      (set! B-list (reverse B-list))))
                (if (= (length B-list) 0)
                    (parser-error))
                (fold-left
                 (lambda (L B)
                   (create-ast 'Ln (list L B)))
                 (create-ast 'Ll (list (car B-list)))
                 (cdr B-list)))))
           (parse-N
            (lambda ()
              (let ((L1 (parse-L)))
                (if (match? #\.)
                    (begin
                      (consume-char)
                      (create-ast 'Nr (list L1 (parse-L))))
                    (create-ast 'Ni (list L1))))))
           (parse-all
            (lambda ()
              (let ((result (parse-N)))
                (if (< pos (string-length input))
                    (parser-error)
                    (create-ast 'S (list result)))))))
      ; Return the compilation function:
      (lambda ()
        (att-value 'v (parse-all)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              User Interface                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an input string encoding a binary number, display the number's decimal
; value or an error message.
(define bin->dec
  (lambda (input)
    (display
     (call/cc
      (lambda (k)
        ((bin->dec-compiler input k)))))))