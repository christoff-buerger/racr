; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets analyses)
 (export specify-analyses pn
         :AtomicPetrinet :Place :Token :Transition :Arc
         ->Place* ->Transition* ->Token* ->In ->Out
         ->name ->value ->place ->consumers ->* <-
         =places =transitions =in-arcs =out-arcs
         =p-lookup =t-lookup =in-lookup =out-lookup =place =valid? =enabled?
         
         :ComposedNet :Glueing :Inport :Outport
         ->Port* ->Glueing*
         ->Net1 ->Net2
         ->outport ->inport
         =ports =glueings =root =<-net =inport? =outport? =find-subnet
         =inport =outport =glued? =fused-places)
 (import (rnrs) (racr core))
 
 (define pn                    (create-specification))
 
 ; AST Accessors:
 (define (->Place* n)          (ast-child 'Place* n))
 (define (->Transition* n)     (ast-child 'Transition* n))
 (define (->Token* n)          (ast-child 'Token* n))
 (define (->In n)              (ast-child 'In n))
 (define (->Out n)             (ast-child 'Out n))
 (define (->name n)            (ast-child 'name n))
 (define (->value n)           (ast-child 'value n))
 (define (->place n)           (ast-child 'place n))
 (define (->consumers n)       (ast-child 'consumers n))
 (define (->* n)               (ast-children n))
 (define (<- n)                (ast-parent n))
 
 (define (Inport? n)           (ast-subtype? n 'Inport))
 (define (Outport? n)          (ast-subtype? n 'Outport))
 (define (->Port* n)           (ast-child 'Port* n))
 (define (->Glueing* n)        (ast-child 'Glueing* n))
 (define (->Net1 n)            (ast-child 'Net1 n))
 (define (->Net2 n)            (ast-child 'Net2 n))
 (define (->outport n)         (ast-child 'outport n))
 (define (->inport n)          (ast-child 'inport n))
 
 ; Attribute Accessors:
 (define (=places n)           (att-value 'places n))
 (define (=transitions n)      (att-value 'transitions n))
 (define (=in-arcs n)          (att-value 'in-arcs n))
 (define (=out-arcs n)         (att-value 'out-arcs n))
 (define (=p-lookup n name)    (hashtable-ref (att-value 'p-lookup n) name #f))
 (define (=t-lookup n name)    (hashtable-ref (att-value 't-lookup n) name #f))
 (define (=in-lookup n name)   (hashtable-ref (att-value 'in-lookup n) name #f))
 (define (=out-lookup n name)  (hashtable-ref (att-value 'out-lookup n) name #f))
 (define (=place n)            (att-value 'place n))
 (define (=valid? n)           (att-value 'valid? n))
 (define (=enabled? n)         (att-value 'enabled? n))
 
 (define (=ports n)            (att-value 'ports n))
 (define (=glueings n)         (att-value 'glueings n))
 (define (=root n)             (att-value 'root n))
 (define (=<-net n)            (att-value '<-net n))
 (define (=inport? n)          (att-value 'inport? n))
 (define (=outport? n)         (att-value 'outport? n))
 (define (=find-subnet n name) (att-value 'find-subnet n name))
 (define (=inport n)           (att-value 'inport n))
 (define (=outport n)          (att-value 'outport n))
 (define (=glued? n . l)       (apply att-value 'glued? n l))
 (define (=fused-places n)     (att-value 'fused-places n))
 
 ; AST Constructors:
 (define (:AtomicPetrinet p t i) ; BEWARE: Redefinition
   (create-ast pn 'AtomicPetrinet
               (list (create-ast-list p) (create-ast-list t) (create-ast-list i))))
 (define (:Place n . t)
   (create-ast pn 'Place (list n (create-ast-list t))))
 (define (:Token v)
   (create-ast pn 'Token (list v)))
 (define (:Transition n i o)
   (create-ast pn 'Transition (list n (create-ast-list i) (create-ast-list o))))
 (define (:Arc p f)
   (create-ast pn 'Arc (list p f)))
 
 (define (:ComposedNet n1 n2)
   (create-ast pn 'ComposedNet (list n1 n2)))
 (define (:Glueing o i)
   (create-ast pn 'Glueing (list o i)))
 (define (:Inport p)
   (create-ast pn 'Inport (list p)))
 (define (:Outport p)
   (create-ast pn 'Outport (list p)))
 
 ; Support Functions:
 (define (make-symbol-table decls ->key . conditions) ; BEWARE: Redefinition
   (define table (make-eq-hashtable))
   (for-each
    (lambda (n)
      (when (for-all (lambda (c) (c n)) conditions)
        (hashtable-set! table (->key n) n)))
    decls)
   table)
 
 (define (specify-analyses)
   (with-specification
    pn
    
    ;;; AST Scheme:
    
    (ast-rule 'AtomicPetrinet:Petrinet->name-Place*-Transition*-Port*) ; BEWARE: Redefinition
    (ast-rule 'Place->name-Token*)
    (ast-rule 'Token->value)
    (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
    (ast-rule 'Arc->place-consumers)
    
    (ast-rule 'Petrinet->)
    (ast-rule 'ComposedNet:Petrinet->Petrinet<Net1-Petrinet<Net2-Glueing*)
    (ast-rule 'Glueing->outport-inport)
    (ast-rule 'Port->place)
    (ast-rule 'Inport:Port->)
    (ast-rule 'Outport:Port->)
    
    (compile-ast-specifications 'AtomicPetrinet)
    
    ;;; Query Support:
    
    (ag-rule places      (AtomicPetrinet (lambda (n) (->* (->Place* n)))))
    (ag-rule transitions (AtomicPetrinet (lambda (n) (->* (->Transition* n)))))
    (ag-rule in-arcs     (Transition     (lambda (n) (->* (->In n)))))
    (ag-rule out-arcs    (Transition     (lambda (n) (->* (->Out n)))))
    
    (ag-rule ports       (AtomicPetrinet (lambda (n) (->* (->Port* n)))))
    (ag-rule glueings    (ComposedNet    (lambda (n) (->* (->Glueing* n)))))
    (ag-rule root        (Petrinet       (lambda (n) (if (ast-has-parent? n) (=root (<- n)) n))))
    
    ;;; Name Analysis:
    
    (ag-rule place       (Arc            (lambda (n) (=p-lookup n (->place n)))))
    (ag-rule p-lookup    (AtomicPetrinet (lambda (n) (make-symbol-table (=places n) ->name))))
    (ag-rule t-lookup    (AtomicPetrinet (lambda (n) (make-symbol-table (=transitions n) ->name))))
    (ag-rule in-lookup   (Transition     (lambda (n) (make-symbol-table (=in-arcs n) ->place))))
    (ag-rule out-lookup  (Transition     (lambda (n) (make-symbol-table (=out-arcs n) ->place))))
    
    (ag-rule place       (Port           (lambda (n) (=p-lookup n (->place n)))))
    (ag-rule inport?     (Place          (lambda (n) (=in-lookup n (->name n)))))
    (ag-rule outport?    (Place          (lambda (n) (=out-lookup n (->name n)))))
    (ag-rule in-lookup   (AtomicPetrinet (lambda (n) (make-symbol-table (=ports n) ->place Inport?))))
    (ag-rule out-lookup  (AtomicPetrinet (lambda (n) (make-symbol-table (=ports n) ->place Outport?))))
    (ag-rule find-subnet (AtomicPetrinet (lambda (n name) (and (eq? (->name n) name) n))))
    (ag-rule find-subnet (ComposedNet    (lambda (n name) (or (=find-subnet (->Net1 n) name)
                                                              (=find-subnet (->Net2 n) name)))))
    
    ;;; Composition Analysis:
    
    (ag-rule
     glued? ; Is the port glued (return its glueing if so)?
     (Port           (lambda (n) (=glued? (<- n) n)))
     (Glueing        (lambda (n p) (or (eq? (=inport n) p) (eq? (=outport n) p))))
     (AtomicPetrinet (lambda (n p) (and (ast-has-parent? n) (=glued? (<- n) p))))
     (ComposedNet    (lambda (n p) (or (find (lambda (n) (=glued? n p)) (=glueings n))
                                       (and (ast-has-parent? n) (=glued? (<- n) p))))))
    
    (ag-rule inport  (Glueing (lambda (n) (let ((net (=find-subnet (car (->inport n)))))
                                            (and net (=in-lookup net (cdr (->inport n))))))))
    (ag-rule outport (Glueing (lambda (n) (let ((net (=find-subnet (car (->outport n)))))
                                            (and net (=out-lookup net (cdr (->outport n))))))))

    (ag-rule
     fused-places
     (Place 'TODO))
    
    ;;; Well-formedness Analysis:
    
    (ag-rule
     valid?
     (Place              (lambda (n) (and (eq? (=p-lookup n (->name n)) n) ; BEWARE: Redefinition
                                          (for-all (lambda (f) (not (eq? (=<-net f) (=<-net n))))
                                            (remq n (=fused-places n))))))
     (Transition         (lambda (n) (and (eq? (=t-lookup n (->name n)) n)
                                          (for-all =valid? (=in-arcs n))
                                          (for-all =valid? (=out-arcs n)))))
     ((Transition In)    (lambda (n) (and (=place n) (eq? (=in-lookup n (->place n)) n))))
     ((Transition Out)   (lambda (n) (and (=place n) (eq? (=out-lookup n (->place n)) n))))
     (AtomicPetrinet     (lambda (n) (and (for-all =valid? (=places n)) ; BEWARE: Redefinition
                                          (for-all =valid? (=transitions n))
                                          (for-all =valid? (=ports n))
                                          (eq? (=find-subnet (=root n) (->name n)) n)))))
    
    (ag-rule
     valid?
     (Inport             (lambda (n) (and (=place n) (eq? (=in-lookup n (->place n)) n))))
     (Outport            (lambda (n) (and (=place n) (eq? (=out-lookup n (->place n)) n))))
     (Glueing            (lambda (n) (let ((in (=inport n)) (out (=outport n)))
                                       (and in out (eq? (=glued? in) n) (eq? (=glued? out) n)))))
     (ComposedNet        (lambda (n) (and (=valid? (->Net1 n))
                                          (=valid? (->Net2 n))
                                          (for-all =valid? (=glueings n))))))
    
    ;;; Enabled Analysis:
    
    (ag-rule
     enabled?
     
     (Arc
      (lambda (n)
        (define consumed (list))
        (define (find-consumable f)
          (ast-find-child
           (lambda (i n)
             (let ((enabled? (and (not (memq n consumed)) (f (->value n)) n)))
               (when enabled? (set! consumed (cons n consumed)))
               enabled?))
           (->Token* (=place n))))
        (call/cc
         (lambda (abort)
           (fold-left
            (lambda (result f)
              (define consumed? (find-consumable f))
              (if consumed? (cons consumed? result) (abort #f)))
            (list)
            (->consumers n))))))
     ;(set!
     ; consumed
     ; (map find-consumable (->consumers n)))
     ;(and (for-all (lambda (x) x) consumed) consumed)))
     
     (Transition
      (lambda (n)
        ;(define result (list))
        ;(and
        ; (not
        ;  (ast-find-child
        ;   (lambda (i n)
        ;     (let ((enabled? (=enabled? n)))
        ;       (and enabled? (begin (set! result (append result enabled?)) #f))))
        ;   (->In n)))
        ; result)
        (and
         (not (ast-find-child (lambda (i n) (not (=enabled? n))) (->In n)))
         (fold-left
          (lambda (result n)
            (append result (=enabled? n)))
          (list)
          (=in-arcs n)))))))))