; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple type)
 (export
  type-rtype
  type-paras
  type-boolean?
  type-number?
  type-integer?
  type-real?
  type-pointer?
  type-procedure?
  type-undefined?
  type-error-type?
  type-boolean
  type-integer
  type-real
  type-pointer
  type-procedure
  type-undefined
  type-error-type
  type-eq?
  type-beq?
  type-pretty-print)
 (import (rnrs))
 
 (define-record-type type
   (fields domain rtype paras))
 
 (define type-boolean?
   (lambda (t) (eq? (type-domain t) 'Boolean)))
 (define type-number?
   (lambda (t) (or (type-integer? t) (type-real? t))))
 (define type-integer?
   (lambda (t) (eq? (type-domain t) 'Integer)))
 (define type-real?
   (lambda (t) (eq? (type-domain t) 'Real)))
 (define type-pointer?
   (lambda (t) (eq? (type-domain t) 'Pointer)))
 (define type-procedure?
   (lambda (t) (eq? (type-domain t) 'Procedure)))
 (define type-undefined?
   (lambda (t) (eq? (type-domain t) 'undefined)))
 (define type-error-type?
   (lambda (t) (eq? (type-domain t) 'error-type)))
 
 (define type-boolean
   (lambda () (make-type 'Boolean #f (list))))
 (define type-integer
   (lambda () (make-type 'Integer #f (list))))
 (define type-real
   (lambda () (make-type 'Real #f (list))))
 (define type-pointer
   (lambda (pointed-to-type) (make-type 'Pointer pointed-to-type (list))))
 (define type-procedure
   (lambda (rtype paras) (make-type 'Procedure rtype paras)))
 (define type-undefined
   (lambda () (make-type 'undefined #f (list))))
 (define type-error-type
   (lambda () (make-type 'error-type #f (list))))
 
 (define type-eq?
   (lambda (t1 t2)
     (cond
       ((not (eq? (type-domain t1) (type-domain t2)))
        (type-error-type))
       ((or
         (and (type-rtype t1) (not (type-rtype t2)))
         (and (not (type-rtype t1)) (type-rtype t2))
         (and
          (type-rtype t1)
          (not (type-beq? (type-rtype t1) (type-rtype t2)))))
        (type-error-type))
       ((or (not (= (length (type-paras t1)) (length (type-paras t2))))
            (find
             (lambda (pair)
               (not (type-beq? (car pair) (cdr pair))))
             (map cons (type-paras t1) (type-paras t2))))
        (type-error-type))
       (else t1))))
 
 (define type-beq?
   (lambda (t1 t2)
     (not (type-error-type? (type-eq? t1 t2)))))
 
 (define type-pretty-print
   (lambda (t)
     (let* ((rtype (type-rtype t))
            (paras (type-paras t))
            (paras? (> (length paras) 0)))
       (string-append
        (symbol->string (type-domain t))
        (cond
          ((type-pointer? t)
           (string-append "(" (type-pretty-print rtype) ")"))
          ((type-procedure? t)
           (string-append
            "("
            (fold-left
             (lambda (result para)
               (string-append result ", " (type-pretty-print para)))
             (if paras?
                 (type-pretty-print (car paras))
                 "")
             (if paras?
                 (cdr paras)
                 (list)))
            (if (type-undefined? rtype)
                ""
                (string-append
                 (if paras?
                     " : "
                     ":")
                 (type-pretty-print rtype)))
            ")"))
          (else "")))))))