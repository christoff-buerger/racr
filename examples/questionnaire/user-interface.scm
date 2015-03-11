; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (questionnaire user-interface)
 (export ql ->Body ->Expression ->name ->label ->type ->value ->operator ->Operands ->* <- index
         count Root Error-question Error-question? G-Lookup L-Lookup Type Valid? L-Valid? S-Expr
         Find-active Active? Shown? Value Widget Render Boolean Number String ErrorType && // !=
         Form If ?? ~? ~> ~! ~~ load-questionnaire save-questionnaire)
 (import (rnrs) (rnrs eval) (racr core) (racket class) (racket format) (racket gui base))
 
 (define ql                   (create-specification))
 (define ql-env               (environment '(rnrs) '(questionnaire user-interface)))
 
 ; AST Accessors:
 (define (->Body n)           (ast-child 'Body n))
 (define (->Expression n)     (ast-child 'Expression n))
 (define (->name n)           (ast-child 'name n))
 (define (->label n)          (ast-child 'label n))
 (define (->type n)           (ast-child 'type n))
 (define (->value n)          (ast-child 'value n))
 (define (->operator n)       (ast-child 'operator n))
 (define (->Operands n)       (ast-child 'Operands n))
 (define (->* n)              (ast-children n))
 (define (<- n)               (ast-parent n))
 (define (index n)            (ast-child-index n))
 (define (count n)            (ast-num-children n))
 
 ; Attribute Accessors:
 (define (Root n)             (att-value 'Root n))
 (define (Error-question n)   (att-value 'Error-question n))
 (define (Error-question? n)  (att-value 'Error-question? n))
 (define (G-Lookup n name)    (att-value 'G-Lookup n name))
 (define (L-Lookup n name)    (att-value 'L-Lookup n name))
 (define (Type n)             (att-value 'Type n))
 (define (Valid? n)           (att-value 'Valid? n))
 (define (L-Valid? n)         (att-value 'L-Valid? n))
 (define (S-Expr n)           (att-value 'S-Expr n))
 (define (Find-active n name) (att-value 'Find-active n name))
 (define (Active? n)          (att-value 'Active? n))
 (define (Shown? n)           (att-value 'Shown? n))
 (define (Value n)            (att-value 'Value n))
 (define (Widget n)           (att-value 'Widget n))
 (define (Render n)           (att-value 'Render n))
 
 ; Type Support:
 (define (Boolean)            (list 'Boolean))
 (define (Number)             (list 'Number))
 (define (String)             (list 'String))
 (define (ErrorType)          (list 'ErrorType))
 (define (valid-type! t)
   (if (memq t (list Boolean Number String)) t (raise "Unknown type.")))
 
 ; Operator support:
 (define (&& . a)             (for-all (lambda (x) x) a))
 (define (// . a)             (find (lambda (x) x) a))
 (define (!= . a)
   (or (null? a) (and (not (memq (car a) (cdr a))) (apply != (cdr a)))))
 
 ; AST Constructors:
 (define (Form . e)
   (create-ast ql 'Form (list (create-ast-list (cons (~? ErrorType "" (~! #f)) e)))))
 (define (If c . e)
   (create-ast ql 'Group (list c (create-ast-list e))))
 (define ??
   (case-lambda
     ((n l t)
      (create-ast ql 'OrdinaryQuestion (list n l (valid-type! t) #f)))
     ((n l t v)
      (create-ast ql 'OrdinaryQuestion (list n l (valid-type! t) v)))))
 (define (~? n l e)
   (create-ast ql 'ComputedQuestion (list n l e)))
 (define (~> n)
   (create-ast ql 'Use (list n)))
 (define (~! v)
   (create-ast ql 'Constant (list v)))
 (define (~~ o . a)
   (create-ast ql 'Computation (list o (create-ast-list a))))
 
 ;;; Loading & saving:
 
 (define (load-questionnaire)
   (define (update-questions n) ; Set the initial value the widgets of ordinary questions show.
     (case (ast-node-type n)
       ((Form Group) (for-each update-questions (->* (->Body n))))
       ((ComputedQuestion) #f)
       (else (send (Widget n) set-value (if (eq? (Type n) Boolean) (Value n) (~a (Value n)))))))
   (define file? (get-file "Select questionnaire" #f #f #f #f (list) (list)))
   (and file?
        (let ((form (eval (with-input-from-file file? (lambda () (read))) ql-env)))
          (update-questions form)
          (Render form)
          form)))
 
 (define (save-questionnaire form)
   (define file? (put-file "Select questionnaire" #f #f #f #f (list) (list)))
   (when file?
     (when (file-exists? file?) (delete-file file?))
     (with-output-to-file file? (lambda () (write (S-Expr form)))))))