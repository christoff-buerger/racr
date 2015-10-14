(import
   (rnrs)
   (srfi :19)
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
        (att-value 'Eval (ast-child 'Exp n))))
    (AddExp
      (lambda (n)
        (+
          (att-value 'Eval (ast-child 'A n))
          (att-value 'Eval (ast-child 'B n)))))
    (MulExp
      (lambda (n)
        (*
          (att-value 'Eval (ast-child 'A n))
          (att-value 'Eval (ast-child 'B n)))))

    (Number
      (lambda (n)
        (ast-child 'value n)))

    (Const
      (lambda (n)
        (ast-child 'value (att-value 'Lookup n (ast-child 'name n))))))

  (ag-rule
    Lookup
    (Root
      (lambda (n name)
        (ast-find-child
          (lambda (i d)
            (string=? (ast-child 'name d) name))
          (ast-child 'Defs n)))))

  (compile-ag-specifications))


(define defs
  (create-ast-list
    (list
      (create-ast spec 'Def (list "a"  0.0))
      (create-ast spec 'Def (list "b"  1.0))
      (create-ast spec 'Def (list "c"  2.0))
      (create-ast spec 'Def (list "d"  3.0))
      (create-ast spec 'Def (list "e"  4.0))
      (create-ast spec 'Def (list "f"  5.0))
      (create-ast spec 'Def (list "g"  6.0))
      (create-ast spec 'Def (list "h"  7.0))
      (create-ast spec 'Def (list "i"  8.0))
      (create-ast spec 'Def (list "j"  9.0))
      (create-ast spec 'Def (list "k" 10.0))
      (create-ast spec 'Def (list "l" 11.0))
      (create-ast spec 'Def (list "m" 12.0))
      (create-ast spec 'Def (list "n" 13.0))
      (create-ast spec 'Def (list "o" 14.0))
      (create-ast spec 'Def (list "p" 15.0))
      (create-ast spec 'Def (list "q" 16.0))
      (create-ast spec 'Def (list "r" 17.0))
      (create-ast spec 'Def (list "s" 18.0))
      (create-ast spec 'Def (list "t" 19.0))
      (create-ast spec 'Def (list "u" 20.0))
      (create-ast spec 'Def (list "v" 21.0))
      (create-ast spec 'Def (list "w" 22.0))
      (create-ast spec 'Def (list "x" 23.0))
      (create-ast spec 'Def (list "y" 24.0))
      (create-ast spec 'Def (list "z" 25.0)))))

(define e
  (tree spec))

(define root
  (create-ast spec 'Root (list defs e)))

(display "Start\n")
(define t (current-time))


(let loop ((i 0))
  (let
    ((def (ast-child (+ (mod i 26) 1) defs)))
    (display i)
    (display ": ")
    (display (att-value 'Eval root))
    (newline)
    (rewrite-terminal 'value def (mod (+ (ast-child 'value def) 1.0) 26.0))
    (when (< i 1000) (loop (+ i 1)))))

(display "time: ")
(display (* (time-nanosecond (time-difference (current-time) t)) 0.000001))
(newline)
