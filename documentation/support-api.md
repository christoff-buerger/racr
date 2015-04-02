_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Support API

### `with-specification`

```
(with-specification
 expression-yielding-specification
 ; Arbitrary many further expressions:
 ...)
```

Syntax definition which eases the use of common _RACR_ library functions by providing an environment where mandatory _RACR_ specification parameters are already bound to a given specification. The `with-specification` form defines for every _RACR_ function with a specification parameter an equally named version without the specification parameter and uses the value of its first expression argument as default specification for the newly defined functions (colloquially explained, it rebinds the _RACR_ functions with specification parameters to simplified versions where the specification parameters are already bound). The scope of the simplified functions are the expressions following the first one. Similarly to the `begin` form, `with-specification` evaluates each of its expression arguments in sequence and returns the value of its last argument. If the value of the last argument is not defined, also the value of `with-specification` is not defined.

```
(assert
 (=
  (att-value
   'length
   (with-specification
    (create-specification)
    
    (ast-rule 'S->List)
    (ast-rule 'List->)
    (ast-rule 'NonNil:List->elem-List<Rest)
    (ast-rule 'Nil:List->)
    (compile-ast-specifications 'S)
    
    (ag-rule
     length
     (S
      (lambda (n)
        (att-value 'length (ast-child 'List n))))
     (NonNil
      (lambda (n)
        (+ (att-value 'length (ast-child 'Rest n)) 1)))
     (Nil
      (lambda (n)
        0)))
    (compile-ag-specifications)
    
    (create-ast
     'S
     (list
      (create-ast
       'NonNil
       (list
        1
        (create-ast
         'NonNil
         (list
          2
          (create-ast 'Nil (list))))))))))
  2))
```

### `with-bindings`

```
(with-bindings
 ((association-list-keys ...)
  (parameter-names ...))
 ; Arbitrary code with references to keys and parameters:
 ...)
(with-bindings
 (association-list-keys ...)
 ; Arbitrary code with references to keys:
 ...)
```

Syntax form, that given a list of key variables `k`, an optional list of parameter variables `p` and arbitrary many s-expressions `s` constructs an `1 + |p|` arity function `f` whose body is `s` and which provides for each key in `k` and parameter in `p` a respective binding. The bindings are established as follows: The first argument of the constructed function `f` must be an association list `l`. Each key in `k` is bound to the `cdr` of its respective entry in `l`. The further arguments of `f` are bound to the respective parameter variables in `p` regarding the order of `p` (i.e., the second argument is bound to the first variable in `p`, the third to the second, etc.).

An exception is thrown, if the first argument to the constructed function `f` is not an association list, does not contain a key for a variable in `k` or the number of further arguments does not equal the number of parameter variables in `p`.

**Note:** _`With-bindings` eases the specification of pattern conditions for `specify-pattern` and of transformers for these patterns using `create-transformer-for-pattern`. Using `with-bindings`, developers can denote the nodes bound throughout matching without writing boilerplate code to search through and bind the values of returned binding lists._

```
(assert
 (=
  ((with-bindings
    (A C)
    (+ A C))
   (list (cons 'A 1) (cons 'B 2) (cons 'C 3) (cons 'D 4)))
  4))
(assert
 (=
  ((with-bindings
    ((A B)
     (X Y Z))
    (- (+ A B Z) X Y))
   (list (cons 'A 1) (cons 'B 2))
   100
   200
   1000)
  703))
```

### `specification->phase`

```
(specification->phase spec)
```

Given a _RACR_ specification, return in which [specification phase](architecture.md#api) it currently is. Possible return values are:
  * AST specification phase: `1`
  * AG specification phase:  `2`
  * Evaluation phase: `3`

```
(let ((spec (create-specification)))
  (assert (= (specification->phase spec) 1))
  (ast-rule spec 'S->)
  (compile-ast-specifications spec 'S)
  (assert (= (specification->phase spec) 2))
  (compile-ag-specifications spec)
  (assert (= (specification->phase spec) 3)))
```
