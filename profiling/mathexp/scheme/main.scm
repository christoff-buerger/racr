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
      (create-ast spec 'Def (list "a" 0.0))
      (create-ast spec 'Def (list "b" 0.1))
      (create-ast spec 'Def (list "c" 0.2))
      (create-ast spec 'Def (list "d" 0.3))
      (create-ast spec 'Def (list "e" 0.4))
      (create-ast spec 'Def (list "f" 0.5))
      (create-ast spec 'Def (list "g" 0.6))
      (create-ast spec 'Def (list "h" 0.7))
      (create-ast spec 'Def (list "i" 0.8))
      (create-ast spec 'Def (list "j" 0.9))
      (create-ast spec 'Def (list "k" 1.0))
      (create-ast spec 'Def (list "l" 1.1))
      (create-ast spec 'Def (list "m" 1.2))
      (create-ast spec 'Def (list "n" 1.3))
      (create-ast spec 'Def (list "o" 1.4))
      (create-ast spec 'Def (list "p" 1.5))
      (create-ast spec 'Def (list "q" 1.6))
      (create-ast spec 'Def (list "r" 1.7))
      (create-ast spec 'Def (list "s" 1.8))
      (create-ast spec 'Def (list "t" 1.9))
      (create-ast spec 'Def (list "u" 2.0))
      (create-ast spec 'Def (list "v" 2.1))
      (create-ast spec 'Def (list "w" 2.2))
      (create-ast spec 'Def (list "x" 2.3))
      (create-ast spec 'Def (list "y" 2.4))
      (create-ast spec 'Def (list "z" 2.5)))))

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
    (rewrite-terminal 'value def (mod (+ (ast-child 'value def) 0.1) 3.0))
    (when (< i 1000) (loop (+ i 1)))))

(define dif (time-difference (current-time) t))

(display "Time: ")
(display (+ (time-second dif) (* (time-nanosecond dif) 0.000000001)))
(newline)
