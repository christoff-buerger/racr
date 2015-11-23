; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (questionnaires language)
 (export ql
         Form If ?? ~? ~> ~! ~~
         ->Body ->Expression ->Operands
         ->name ->label ->type ->value ->operator ->* <- index count
         =root =error-question =error-question? =g-Lookup =l-Lookup =type =valid? =l-valid?
         =s-expr =find-active =active? =shown? =value =widget =render
         Boolean Number String ErrorType && // !=
         load-questionnaire save-questionnaire)
 (import (rnrs) (rnrs eval) (compatibility mlist) (racr core)
         (prefix (racket base) r:) (racket format) (racket class) (racket gui base))
 
 (define ql                    (create-specification))
 (define ql-env                (environment '(rnrs) '(questionnaires language)))
 
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
 
 ; Loading & saving:
 
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
     (with-output-to-file file? (lambda () (write (=s-expr form))))))
 
 (with-specification
  ql
  
  ;;; AST Scheme:
  
  (ast-rule 'Form->Element*<Body)
  (ast-rule 'Element->)
  (ast-rule 'Group:Element->Expression-Element*<Body)
  (ast-rule 'Question:Element->name-label)
  (ast-rule 'OrdinaryQuestion:Question->type-value)
  (ast-rule 'ComputedQuestion:Question->Expression)
  (ast-rule 'Expression->)
  (ast-rule 'Use:Expression->name)
  (ast-rule 'Constant:Expression->value)
  (ast-rule 'Computation:Expression->operator-Expression*<Operands)
  (compile-ast-specifications 'Form)
  
  ;;; Support Attributes:
  
  (ag-rule
   root ; The root of a questionnaire.
   (Form             (lambda (n) n)))
  
  (ag-rule
   error-question ; Question representing undeclared uses.
   (Form             (lambda (n) (ast-child 1 (->Body n)))))
  
  (ag-rule
   error-question? ; Is a question the error question?
   (Element          (lambda (n) (eq? n (=error-question n)))))
  
  ;;; Name Analysis:
  
  (define (find-L name l i)
    (ast-find-child*
     (lambda (i e) (=l-Lookup e name))
     l (cons 1 i)))
  
  (ag-rule
   g-Lookup ; Find question w.r.t. context (error question if no hit, declare before use).
   ((Form Body)
    (lambda (n name)
      (or (find-L name (<- n) (- (index n) 1))
          (=error-question n))))
   ((Group Body)
    (lambda (n name)
      (or (find-L name (<- n) (- (index n) 1))
          (=g-Lookup (<- n) name)))))
  
  (ag-rule
   l-Lookup ; Find question in subtree (#f if no hit).
   (Question         (lambda (n name) (if (eq? (->name n) name) n #f)))
   (Group            (lambda (n name) (find-L name (->Body n) (count (->Body n))))))
  
  ;;; Type Analysis:
  
  (ag-rule
   type ; Type of questions & expressions.
   (OrdinaryQuestion (lambda (n) (->type n)))
   (ComputedQuestion (lambda (n) (=type (->Expression n))))
   (Use              (lambda (n) (=type (=g-Lookup n (->name n)))))
   
   (Constant
    (lambda (n)
      (cond
        ((boolean? (->value n)) Boolean)
        ((number? (->value n))  Number)
        ((string? (->value n))  String)
        (else ErrorType))))
   
   (Computation
    (lambda (n)
      (let ((ops (->* (->Operands n))))
        (define (in . l) (memq (->operator n) l))
        (define (ensure-type t l)
          (for-all (lambda (n) (eq? (=type n) t)) l))
        (cond
          ((in && // not)
           (if (ensure-type Boolean ops) Boolean ErrorType))
          ((in = < > <= >= !=)
           (if (ensure-type Number ops) Boolean ErrorType))
          ((in + - * /)
           (if (ensure-type Number ops) Number ErrorType))
          ((in string-append)
           (if (ensure-type String ops) String ErrorType))
          ((in string=? string<? string>? string<=? string>=?)
           (if (ensure-type String ops) Boolean ErrorType))
          (else ErrorType))))))
  
  ;;; Well-formedness:
  
  (ag-rule
   valid? ; Is the form well-formed?
   (Form             (lambda (n) (for-all =valid? (cdr (->* (->Body n))))))
   (Group            (lambda (n) (and (=l-valid? n) (for-all =valid? (->* (->Body n))))))
   (Question         =l-valid?))
  
  (ag-rule
   l-valid? ; Is a form element well-formed?
   (Group            (lambda (n) (eq? (=type (->Expression n)) Boolean)))
   (Question
    (lambda (n)
      (and
       (not (eq? (=type n) ErrorType))
       (let ((previous (=g-Lookup n (->name n))))
         (or (=error-question? previous)
             (eq? (=type n) (=type previous))))))))
  
  ;;; Persistency:
  
  (define (: p)      (string->symbol (substring (~a p) 12 (- (string-length (~a p)) 1))))
  (ag-rule
   s-expr ; Symbolic expression representing form.
   (Form             (lambda (n) `(Form ,@(map =s-expr (cdr (->* (->Body n)))))))
   (Group            (lambda (n) `(If ,(=s-expr (->Expression n)) ,@(map =s-expr (->* (->Body n))))))
   (OrdinaryQuestion (lambda (n) `(?? ',(->name n) ,(->label n) ,(: (->type n)) ,(->value n))))
   (ComputedQuestion (lambda (n) `(~? ',(->name n) ,(->label n) ,(=s-expr (->Expression n)))))
   (Use              (lambda (n) `(~> ',(->name n))))
   (Constant         (lambda (n) `(~! ,(->value n))))
   (Computation      (lambda (n) `(~~ ,(: (->operator n)) ,@(map =s-expr (->* (->Operands n)))))))
  
  ;;; Interpretation:
  
  (ag-rule
   find-active ; Find active question (error question if no hit).
   (Element
    (lambda (n name)
      (define (find-active current)
        (or (and (=active? current) current)
            (find-active (=g-Lookup current name))))
      (find-active (=g-Lookup n name)))))
  
  (ag-rule
   active? ; Is a form part active (the error question is active)?
   (Form             (lambda (n) #t))
   (Group            (lambda (n) (=value (->Expression n))))
   (Question
    (lambda (n)
      (or (=error-question? n)
          (and (=active? (<- n))
               (=error-question? (=find-active n (->name n))))))))
  
  (ag-rule
   shown? ; Is a form part shown (the error question is not shown)?
   (Element          (lambda (n) (and (not (=error-question? n)) (=active? n)))))
  
  (define-syntax lift(syntax-rules () ((_ f) (guard (x (error? #f)) f))))
  (ag-rule
   value ; Value of questions & expressions.
   (OrdinaryQuestion (lambda (n) (->value n)))
   (ComputedQuestion (lambda (n) (=value (->Expression n))))
   (Constant         (lambda (n) (->value n)))
   (Use              (lambda (n) (=value (=find-active n (->name n)))))
   (Computation      (lambda (n) (lift (apply (->operator n) (map =value (->* (->Operands n))))))))
  
  ;;; Rendering (GUI):
  
  (ag-rule
   widget ; Widget representing form elements.
   (Form
    (lambda (n)
      (define frame (new frame% [label "Questionnaire"]))
      (define menu-bar (new menu-bar% [parent frame]))
      (define menu (new menu% [parent menu-bar] [label "File"]))
      (new menu-item% [parent menu] [label "Save"] [callback (lambda x (save-questionnaire n))])
      (new menu-item% [parent menu] [label "Load"] [callback (lambda x (load-questionnaire))])
      (unless (=valid? n)
        (new message% [parent frame] [label 'caution]))
      (new vertical-panel% [parent frame])))
   
   (Group
    (lambda (n)
      (new vertical-panel% [parent (=widget (<- n))] [style (r:list 'border)])))
   
   (ComputedQuestion
    (lambda (n)
      (define widget-class (if (eq? (=type n) Boolean) check-box% text-field%))
      (new widget-class [parent (=widget (<- n))] [label (->label n)] [enabled #f])))
   
   (OrdinaryQuestion
    (lambda (n)
      (define widget-class (if (eq? (=type n) Boolean) check-box% text-field%))
      (define (callback widget event)
        (define (prepare-value v)
          (if (eq? (=type n) Number) (r:string->number v) v))
        (rewrite-terminal 'value n (prepare-value (send widget get-value)))
        (=render (=root n)))
      (new widget-class [parent (=widget (<- n))] [label (->label n)] [callback callback]))))
  
  (ag-rule
   render ; Incrementally render form elements.
   (OrdinaryQuestion (lambda (n) #f))
   
   (Form
    (lambda (n)
      (send (=widget n) begin-container-sequence)
      (let ((shown (filter =shown? (->* (->Body n)))))
        (send (=widget n) change-children (lambda x (mlist->list (map =widget shown))))
        (map =render shown))
      (send (send (=widget n) get-parent) show #t)
      (send (=widget n) end-container-sequence)))
   
   (Group
    (lambda (n)
      (let ((shown (filter =shown? (->* (->Body n)))))
        (send (=widget n) change-children (lambda x (mlist->list (map =widget shown))))
        (map =render shown))))
   
   (ComputedQuestion
    (lambda (n)
      (send (=widget n) set-value (if (eq? (=type n) Boolean) (=value n) (~a (=value n)))))))
  
  (compile-ag-specifications)))