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
         =s-expr =find-active =active? =shown? =value
         =dialog-type =dialog-printer =value-printer =widget =render
         Boolean Number String ErrorType && // !=
         load-questionnaire save-questionnaire)
 (import (rnrs) (rnrs eval) (compatibility mlist) (racr core)
         (prefix (racket base) r:) (racket format) (racket class) (racket gui base))
 
 (define ql                    (create-specification))
 (define ql-env                (environment '(rnrs) '(questionnaires language)))
 
 ;;; AST accessors:
 
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
 
 ;;; Attribute accessors:
 
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
 (define (=dialog-type n)      (att-value 'dialog-type n))
 (define (=dialog-printer n)   (att-value 'dialog-printer n))
 (define (=value-printer n)    (att-value 'value-printer n))
 (define (=widget n)           (att-value 'widget n))
 (define (=render n)           (att-value 'render n))
 
 ;;; AST constructors:
 
 (define (Form . e)
   (create-ast ql 'Form (list (create-ast-list (cons (~? (list #f) "" (~! ErrorValue)) e)))))
 (define (If c . e)
   (create-ast ql 'Group (list c (create-ast-list e))))
 (define ??
   (case-lambda
     ((n l t)
      (create-ast ql 'OrdinaryQuestion (list n l t ErrorValue)))
     ((n l t v)
      (create-ast ql 'OrdinaryQuestion (list n l t v)))))
 (define (~? n l e)
   (create-ast ql 'ComputedQuestion (list n l e)))
 (define (~> n)
   (create-ast ql 'Use (list n)))
 (define (~! v)
   (create-ast ql 'Constant (list v)))
 (define (~~ o . a)
   (create-ast ql 'Computation (list o (create-ast-list a))))
 
 ;;; Type & operator support:
 
 (define (Boolean v)           (boolean? v))
 (define (Number v)            (number? v))
 (define (String v)            (string? v))
 (define valid-types           (list Boolean Number String))
 (define (type->acceptor t)    (find (lambda (e) (eq? e t)) valid-types))
 (define (value->type v)       (find (lambda (e) (e v)) valid-types))
 (define (ErrorType)           (list 'ErrorType))
 (define (ErrorValue)          (list 'ErrorValue))
 (define (&& . a)              (for-all (lambda (x) x) a))
 (define (// . a)              (find (lambda (x) x) a))
 (define (!= . a)
   (or (null? a) (and (not (memq (car a) (cdr a))) (apply != (cdr a)))))
 
 ;;; Exceptions:
 
 (define-condition-type ql-error &non-continuable make-ql-error ql-error?)
 
 (define (ql-error: . messages)
   (define (object->string o)
     (call-with-string-output-port (lambda (port) (display o port))))
   (define message
     (fold-left
      (lambda (result m)
        (string-append result (if (string? m) m (string-append " [" (object->string m) "] "))))
      "IMPLEMENTATION ERROR: " messages))
   (raise (condition (make-ql-error) (make-message-condition message))))
 
 ;;; Loading and saving:
 
 (define (load-questionnaire)
   (define (update-questions n) ; Set the initial value the widgets of ordinary questions show.
     (case (ast-node-type n)
       ((Form Group) (for-each update-questions (->* (->Body n))))
       ((ComputedQuestion) #f)
       (else (send (=widget n) set-value ((=dialog-printer n) (=value n))))))
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
  
  ;;; AST scheme:
  
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
  
  ;;; Support attributes:
  
  (ag-rule
   root ; The root of a questionnaire.
   (Form             (lambda (n) n)))
  
  (ag-rule
   error-question ; Question representing undeclared uses.
   (Form             (lambda (n) (ast-child 1 (->Body n)))))
  
  (ag-rule
   error-question? ; Is a question the error question?
   (Element          (lambda (n) (eq? n (=error-question n)))))
  
  ;;; Name analysis:
  
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
  
  ;;; Type analysis:
  
  (ag-rule
   type ; Type of questions & expressions.
   (OrdinaryQuestion (lambda (n) (if (type->acceptor (->type n)) (->type n) ErrorType)))
   (ComputedQuestion (lambda (n) (=type (->Expression n))))
   (Use              (lambda (n) (=type (=g-Lookup n (->name n)))))
   (Constant         (lambda (n) (or (value->type (->value n)) ErrorType)))
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
   (ComputedQuestion (lambda (n) `(~? ',(->name n) ,(->label n) ,(=s-expr (->Expression n)))))
   (Use              (lambda (n) `(~> ',(->name n))))
   (Constant         (lambda (n) `(~! ,(->value n))))
   (Computation      (lambda (n) `(~~ ,(: (->operator n)) ,@(map =s-expr (->* (->Operands n))))))
   (OrdinaryQuestion
    (lambda (n)
      `(?? ',(->name n) ,(->label n) ,(: (->type n))
           ,@(if (eq? (->value n) ErrorValue) (list) (list (->value n)))))))
  
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
   (Group            (lambda (n) (let ((v (=value (->Expression n)))) (and (boolean? v) v))))
   (Question
    (lambda (n)
      (or (=error-question? n)
          (and (=active? (<- n))
               (=error-question? (=find-active n (->name n))))))))
  
  (ag-rule
   shown? ; Is a form part shown (the error question is not shown)?
   (Element          (lambda (n) (and (not (=error-question? n)) (=active? n)))))
  
  (ag-rule
   value ; Value of questions & expressions.
   (ComputedQuestion (lambda (n) (=value (->Expression n))))
   (Constant         (lambda (n) (if (eq? (=type n) ErrorType) ErrorValue (->value n))))
   
   (Use
    (lambda (n)
      (if (or (eq? (=type n) ErrorType) (not (eq? (=type (=find-active n (->name n))) (=type n))))
          ErrorValue (=value (=find-active n (->name n))))))
   
   (OrdinaryQuestion
    (lambda (n)
      (if (or (eq? (=type n) ErrorType) (not ((type->acceptor (=type n)) (->value n))))
          ErrorValue (->value n))))
   
   (Computation
    (lambda (n)
      (or (and (eq? (=type n) ErrorType) ErrorValue)
          (let ((args (map =value (->* (->Operands n)))))
            (find (lambda (value) (eq? value ErrorValue)) args)
            (guard (x (error? ErrorValue)) (apply (->operator n) args)))))))
  
  ;;; Graphical user interface:
  
  (ag-rule
   dialog-type ; Widget type used to represent question.
   (Question
    (lambda (n)
      (cond
        ((eq? (=type n) Boolean) check-box%)
        ((eq? (=type n) String) text-field%)
        ((eq? (=type n) Number) text-field%)
        ((eq? (=type n) ErrorType) text-field%)
        (else (ql-error: "no dialog for" (=type n) "implemented"))))))
  
  (ag-rule
   dialog-printer ; Function pretty printing AST values for the question's dialog.
   (Question
    (lambda (n)
      (cond
        ((eq? (=dialog-type n) check-box%) (lambda (v) (and (boolean? v) v)))
        ((eq? (=dialog-type n) text-field%) ; (lambda (v) (if (eq? v ErrorValue) "" (~a v)))
         (let ((fits? (type->acceptor (=type n))))
           (cond
             ((eq? (=type n) Number) (lambda (v) (if (fits? v) (number->string v) "")))
             ((eq? (=type n) String) (lambda (v) (if (fits? v) v "")))
             ((eq? (=type n) ErrorType) (lambda (v) ""))
             (else (ql-error: "no" (=type n)  "printer for" (=dialog-type n) "implemented")))))
        (else (ql-error: "no dialog-printer for" (=dialog-type n) "implemented"))))))
  
  (ag-rule
   value-printer ; Function pretty printing dialog values for the question's ast.
   (OrdinaryQuestion
    (lambda (n)
      (cond
        ((eq? (=dialog-type n) check-box%) (lambda (v) v))
        ((eq? (=dialog-type n) text-field%)
         (if (eq? (=type n) Number)
             (lambda (v) (let ((number? (r:string->number v))) (if number? number? v)))
             (lambda (v) v)))
        (else (ql-error: "no value-printer for" (=dialog-type n) "implemented"))))))
  
  (ag-rule
   widget ; Widget representing form element.
   (Form
    (lambda (n)
      (define frame (new frame% [label "Questionnaire"] [border 5]))
      (define menu-bar (new menu-bar% [parent frame]))
      (define menu (new menu% [parent menu-bar] [label "File"]))
      (new menu-item% [parent menu] [label "Save"] [callback (lambda x (save-questionnaire n))])
      (new menu-item% [parent menu] [label "Load"] [callback (lambda x (load-questionnaire))])
      (unless (=valid? n)
        (new message% [parent frame] [label 'caution]))
      (new vertical-panel% [parent frame])))
   
   (Group
    (lambda (n)
      (new vertical-panel% [parent (=widget (<- n))] [border 5] [style (r:list 'border)])))
   
   (ComputedQuestion
    (lambda (n)
      (new (=dialog-type n) [parent (=widget (<- n))] [label (->label n)] [enabled #f])))
   
   (OrdinaryQuestion
    (lambda (n)
      (define (callback widget event)
        (rewrite-terminal 'value n ((=value-printer n) (send widget get-value)))
        (=render (=root n)))
      (new (=dialog-type n) [parent (=widget (<- n))] [label (->label n)] [callback callback]))))
  
  (ag-rule
   render ; Incrementally render form.
   (OrdinaryQuestion (lambda (n) #f))
   
   (ComputedQuestion
    (lambda (n)
      (send (=widget n) set-value ((=dialog-printer n) (=value n)))))
   
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
        (map =render shown)))))
  
  (compile-ag-specifications)))