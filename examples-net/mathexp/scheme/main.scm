(import
   (rnrs)
   (racr core)
   (generated))

(define spec (create-specification))

(with-specification spec
		(ast-rule 'Root->Def*<Defs-Exp)
		(ast-rule 'Def->name-value)
		(ast-rule 'Exp->)
		(ast-rule 'BinExp:Exp->Exp<A-Exp<B)
		(ast-rule 'AddExp:BinExp->)
		(ast-rule 'MulExp:BinExp->)
		(ast-rule 'Number:Exp->value)
		(ast-rule 'Const:Exp->name)

  (compile-ast-specifications 'Root)

  (ag-rule
    Eval
    (Root
      (lambda (n)
;        (display "Root Eval\n")
        (att-value 'Eval (ast-child 'Exp n))))
    (AddExp
      (lambda (n)
;        (display "AddExp Eval\n")
        (+
          (att-value 'Eval (ast-child 'A n))
          (att-value 'Eval (ast-child 'B n)))))
    (MulExp
      (lambda (n)
;        (display "MulExp Eval\n")
        (*
          (att-value 'Eval (ast-child 'A n))
          (att-value 'Eval (ast-child 'B n)))))

    (Number
      (lambda (n)
;        (display "Number Eval\n")
        (ast-child 'value n)))

    (Const
      (lambda (n)
;        (display "Const Eval\n")
        (ast-child 'value (att-value 'Lookup n (ast-child 'name n))))))

  (ag-rule
    Lookup
    (Root
      (lambda (n name)
;        (display "Root Lookup\n")
        (ast-find-child
          (lambda (i d)
            (string=? (ast-child 'name d) name))
          (ast-child 'Defs n)))))

  (compile-ag-specifications))


(define defs
  (create-ast-list
    (list
      (create-ast spec 'Def (list "a"  1.0))
      (create-ast spec 'Def (list "b"  2.0))
      (create-ast spec 'Def (list "c"  3.0))
      (create-ast spec 'Def (list "d"  4.0))
      (create-ast spec 'Def (list "e"  5.0))
      (create-ast spec 'Def (list "f"  6.0))
      (create-ast spec 'Def (list "g"  7.0))
      (create-ast spec 'Def (list "h"  8.0))
      (create-ast spec 'Def (list "i"  9.0))
      (create-ast spec 'Def (list "j" 10.0))
      (create-ast spec 'Def (list "k" 11.0))
      (create-ast spec 'Def (list "l" 12.0))
      (create-ast spec 'Def (list "m" 13.0))
      (create-ast spec 'Def (list "n" 14.0))
      (create-ast spec 'Def (list "o" 15.0))
      (create-ast spec 'Def (list "p" 16.0))
      (create-ast spec 'Def (list "q" 17.0))
      (create-ast spec 'Def (list "r" 18.0))
      (create-ast spec 'Def (list "s" 19.0))
      (create-ast spec 'Def (list "t" 20.0))
      (create-ast spec 'Def (list "u" 21.0))
      (create-ast spec 'Def (list "v" 22.0))
      (create-ast spec 'Def (list "w" 23.0))
      (create-ast spec 'Def (list "x" 24.0))
      (create-ast spec 'Def (list "y" 25.0))
      (create-ast spec 'Def (list "z" 26.0)))))

(define e
  (tree spec))

(define root
  (create-ast spec 'Root (list defs e)))

(display "Start\n")

(display (att-value 'Eval root))
(newline)

;(let loop ((i 0))
;  (let
;    ((def (ast-child (+ (mod i 26) 1) defs)))
;    (display (att-value 'Eval root))
;    (newline)
;    (rewrite-terminal 'value def (+ (ast-child 'value def) 1))
;    (when (< i 1000) (loop (+ i 1)))))
;
