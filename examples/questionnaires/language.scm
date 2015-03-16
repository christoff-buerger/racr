; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (questionnaires language)
 (export)
 (import (rnrs) (racr core) (questionnaires user-interface) (compatibility mlist)
         (prefix (racket base) r:) (racket format) (racket class) (racket gui base))
 
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
  
  ;;; Support attributes:
  
  (ag-rule
   Root ; The root of a questionnaire.
   (Form             (lambda (n) n)))
  
  (ag-rule
   Error-question ; Question representing undeclared uses.
   (Form             (lambda (n) (ast-child 1 (->Body n)))))
  
  (ag-rule
   Error-question? ; Is a question the error question?
   (Element          (lambda (n) (eq? n (Error-question n)))))
  
  ;;; Name Analysis:
  
  (define (find-L name l i)
    (ast-find-child*
     (lambda (i e) (L-Lookup e name))
     l (cons 1 i)))
  
  (ag-rule
   G-Lookup ; Find question w.r.t. context (error question if no hit, declare before use).
   ((Form Body)
    (lambda (n name)
      (or (find-L name (<- n) (- (index n) 1))
          (Error-question n))))
   ((Group Body)
    (lambda (n name)
      (or (find-L name (<- n) (- (index n) 1))
          (G-Lookup (<- n) name)))))
  
  (ag-rule
   L-Lookup ; Find question in subtree (#f if no hit).
   (Question         (lambda (n name) (if (eq? (->name n) name) n #f)))
   (Group            (lambda (n name) (find-L name (->Body n) (count (->Body n))))))
  
  ;;; Type Analysis:
  
  (ag-rule
   Type ; Type of questions & expressions.
   (OrdinaryQuestion (lambda (n) (->type n)))
   (ComputedQuestion (lambda (n) (Type (->Expression n))))
   (Use              (lambda (n) (Type (G-Lookup n (->name n)))))
   
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
          (for-all (lambda (n) (eq? (Type n) t)) l))
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
   Valid? ; Is the form well-formed?
   (Form             (lambda (n) (for-all Valid? (cdr (->* (->Body n))))))
   (Group            (lambda (n) (and (L-Valid? n) (for-all Valid? (->* (->Body n))))))
   (Question         L-Valid?))
  
  (ag-rule
   L-Valid? ; Is a form element well-formed?
   (Group            (lambda (n) (eq? (Type (->Expression n)) Boolean)))
   (Question
    (lambda (n)
      (and
       (not (eq? (Type n) ErrorType))
       (let ((previous (G-Lookup n (->name n))))
         (or (Error-question? previous)
             (eq? (Type n) (Type previous))))))))
  
  ;;; Persistency:
  
  (define (: p)      (string->symbol (substring (~a p) 12 (- (string-length (~a p)) 1))))
  (ag-rule
   S-Expr ; Symbolic expression representing form.
   (Form             (lambda (n) `(Form ,@(map S-Expr (cdr (->* (->Body n)))))))
   (Group            (lambda (n) `(If ,(S-Expr (->Expression n)) ,@(map S-Expr (->* (->Body n))))))
   (OrdinaryQuestion (lambda (n) `(?? ',(->name n) ,(->label n) ,(: (->type n)) ,(->value n))))
   (ComputedQuestion (lambda (n) `(~? ',(->name n) ,(->label n) ,(S-Expr (->Expression n)))))
   (Use              (lambda (n) `(~> ',(->name n))))
   (Constant         (lambda (n) `(~! ,(->value n))))
   (Computation      (lambda (n) `(~~ ,(: (->operator n)) ,@(map S-Expr (->* (->Operands n)))))))
  
  ;;; Interpretation:
  
  (ag-rule
   Find-active ; Find active question (error question if no hit).
   (Element
    (lambda (n name)
      (define (find-active current)
        (or (and (Active? current) current)
            (find-active (G-Lookup current name))))
      (find-active (G-Lookup n name)))))
  
  (ag-rule
   Active? ; Is a form part active (the error question is active)?
   (Form             (lambda (n) #t))
   (Group            (lambda (n) (Value (->Expression n))))
   (Question
    (lambda (n)
      (or (Error-question? n)
          (and (Active? (<- n))
               (Error-question? (Find-active n (->name n))))))))
  
  (ag-rule
   Shown? ; Is a form part shown (the error question is not shown)?
   (Element          (lambda (n) (and (not (Error-question? n)) (Active? n)))))
  
  (define-syntax lift(syntax-rules () ((_ f) (guard (x (error? #f)) f))))
  (ag-rule
   Value ; Value of questions & expressions.
   (OrdinaryQuestion (lambda (n) (->value n)))
   (ComputedQuestion (lambda (n) (Value (->Expression n))))
   (Constant         (lambda (n) (->value n)))
   (Use              (lambda (n) (Value (Find-active n (->name n)))))
   (Computation      (lambda (n) (lift (apply (->operator n) (map Value (->* (->Operands n))))))))
  
  ;;; Rendering (GUI):
  
  (ag-rule
   Widget ; Widget representing form elements.
   (Form
    (lambda (n)
      (define frame (new frame% [label "Questionnaire"]))
      (define menu-bar (new menu-bar% [parent frame]))
      (define menu (new menu% [parent menu-bar] [label "File"]))
      (new menu-item% [parent menu] [label "Save"] [callback (lambda x (save-questionnaire n))])
      (new menu-item% [parent menu] [label "Load"] [callback (lambda x (load-questionnaire))])
      (unless (Valid? n)
        (new message% [parent frame] [label 'caution]))
      (new vertical-panel% [parent frame])))
   
   (Group
    (lambda (n)
      (new vertical-panel% [parent (Widget (<- n))] [style (r:list 'border)])))
   
   (ComputedQuestion
    (lambda (n)
      (define widget-class (if (eq? (Type n) Boolean) check-box% text-field%))
      (new widget-class [parent (Widget (<- n))] [label (->label n)] [enabled #f])))
   
   (OrdinaryQuestion
    (lambda (n)
      (define widget-class (if (eq? (Type n) Boolean) check-box% text-field%))
      (define (callback widget event)
        (define (prepare-value v)
          (if (eq? (Type n) Number) (r:string->number v) v))
        (rewrite-terminal 'value n (prepare-value (send widget get-value)))
        (Render (Root n)))
      (new widget-class [parent (Widget (<- n))] [label (->label n)] [callback callback]))))
  
  (ag-rule
   Render ; Incrementally render form elements.
   (OrdinaryQuestion (lambda (n) #f))
   
   (Form
    (lambda (n)
      (send (Widget n) begin-container-sequence)
      (let ((shown (filter Shown? (->* (->Body n)))))
        (send (Widget n) change-children (lambda x (mlist->list (map Widget shown))))
        (map Render shown))
      (send (send (Widget n) get-parent) show #t)
      (send (Widget n) end-container-sequence)))
   
   (Group
    (lambda (n)
      (let ((shown (filter Shown? (->* (->Body n)))))
        (send (Widget n) change-children (lambda x (mlist->list (map Widget shown))))
        (map Render shown))))
   
   (ComputedQuestion
    (lambda (n)
      (send (Widget n) set-value (if (eq? (Type n) Boolean) (Value n) (~a (Value n)))))))
  
  (compile-ag-specifications)))