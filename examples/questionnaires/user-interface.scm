; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (questionnaires user-interface)
 (export ql ->Body ->Expression ->name ->label ->type ->value ->operator ->Operands ->* <- index
         count =root =error-question =error-question? =g-Lookup =l-Lookup =type =valid? =l-valid?
         =s-expr =find-active =active? =shown? =value =widget =render Boolean Number String
         ErrorType && // != Form If ?? ~? ~> ~! ~~ load-questionnaire save-questionnaire)
 (import (rnrs) (rnrs eval) (racr core) (racket class) (racket format) (racket gui base))
 
 (define ql                    (create-specification))
 (define ql-env                (environment '(rnrs) '(questionnaires user-interface)))
 
 ; AST Accessors:
 (define (->Body n)            (ast-child 'Body n))
 (define (->Expression n)      (ast-child 'Expression n))
 (define (->name n)            (ast-child 'name n))
 (define (->label n)           (ast-child 'label n))
 (define (->type n)            (ast-child 'type n))
 (define (->value n)           (ast-child 'value n))
 (define (->operator n)        (ast-child 'operator n))
 (define (->Operands n)        (ast-child 'Operands n))
 (define (->* n)               (ast-children n))
 (define (<- n)                (ast-parent n))
 (define (index n)             (ast-child-index n))
 (define (count n)             (ast-num-children n))
 
 ; Attribute Accessors:
 (define (=root n)             (att-value 'root n))
 (define (=error-question n)   (att-value 'error-question n))
 (define (=error-question? n)  (att-value 'error-question? n))
 (define (=g-Lookup n name)    (att-value 'g-Lookup n name))
 (define (=l-Lookup n name)    (att-value 'l-Lookup n name))
 (define (=type n)             (att-value 'type n))
 (define (=valid? n)           (att-value 'valid? n))
 (define (=l-valid? n)         (att-value 'l-valid? n))
 (define (=s-expr n)           (att-value 's-expr n))
 (define (=find-active n name) (att-value 'find-active n name))
 (define (=active? n)          (att-value 'active? n))
 (define (=shown? n)           (att-value 'shown? n))
 (define (=value n)            (att-value 'value n))
 (define (=widget n)           (att-value 'widget n))
 (define (=render n)           (att-value 'render n))
 
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

 ; Type & Operator Support:
 (define (Boolean)             (list 'Boolean))
 (define (Number)              (list 'Number))
 (define (String)              (list 'String))
 (define (ErrorType)           (list 'ErrorType))
 (define (valid-type! t)       (if (memq t (list Boolean Number String))
                                   t (raise "Unknown type.")))
 (define (&& . a)              (for-all (lambda (x) x) a))
 (define (// . a)              (find (lambda (x) x) a))
 (define (!= . a)
   (or (null? a) (and (not (memq (car a) (cdr a))) (apply != (cdr a)))))
 
 ;;; Loading & saving:
 
 (define (load-questionnaire)
   (define (update-questions n) ; Set the initial value the widgets of ordinary questions show.
     (case (ast-node-type n)
       ((Form Group) (for-each update-questions (->* (->Body n))))
       ((ComputedQuestion) #f)
       (else (send (=widget n) set-value (if (eq? (=type n) Boolean) (=value n) (~a (=value n)))))))
   (define file? (get-file "Select questionnaire" #f #f #f #f (list) (list)))
   (and file?
        (let ((form (eval (with-input-from-file file? (lambda () (read))) ql-env)))
          (update-questions form)
          (=render form)
          form)))
 
 (define (save-questionnaire form)
   (define file? (put-file "Select questionnaire" #f #f #f #f (list) (list)))
   (when file?
     (when (file-exists? file?) (delete-file file?))
     (with-output-to-file file? (lambda () (write (=s-expr form)))))))