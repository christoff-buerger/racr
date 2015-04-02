_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Abstract Syntax Trees

This chapter presents _RACR's_ abstract syntax tree (AST) API, which provides functions for the specification of AST schemes, the construction of respective ASTs and the querying of ASTs for structural and node information. _RACR_ ASTs are based on the following context-free grammar (CFG), Extended Backus-Naur Form (EBNF) and object-oriented concepts:

  * **CFG** Non-terminals, terminals, productions, total order of production symbols
  * **EBNF** Unbounded repetition (Kleene Star)
  * **Object-Oriented Programming** Inheritance, named fields

_RACR_ ASTs are directed, typed, ordered trees. Every AST node has a type, called its node type, and a finite number of children. Every child has a name and represents either, another AST node spanning a subtree or an arbitrary _Scheme_ value. If it spans a subtree, it has a node type and is called a non-terminal child of the respective type, otherwise a terminal child. Non-terminal children can be unbounded repetitions, which means instead of a single node of the non-terminal's type the child is a so called list node that has arbitrary many children of the respective type. The children of a node type must have different names; children of different node types can have equal names. We call names defined for children context names and a node with type `T` an instance of `T`. Given a node, the number, order, types, names and information, whether they are unbounded repetitions, of its children are induced by its type.

Node types can inherit from each other. If a node type `A` inherits from another type `B`, `A` is called direct subtype of `B` and `B` direct supertype of `A`. The transitive closure of direct sub- and supertype are called a node type's sub- and supertypes, i.e., a node type `A` is a sub-/supertype of a type `B`, if `A` is a direct sub-/supertype of `B` or `A` is a direct sub-/supertype of a type `C` that is a sub-/supertype of `B`. Node types can inherit from atmost one other type and must not be subtypes of themselves. If a node type is subtype of another one, its instances can be used anywhere an instance of its supertype is expected, i.e., if `A` is a subtype of `B`, every AST node of type `A` also is of type `B`. The children induced by a node type are the ones of its direct supertype, if it has any, followed by the ones specified for itself.

Node types are specified using AST rules. The set of all AST rules of a _RACR_ specification are called an AST scheme. Every AST rule specifies a node type of a certain name. AST rules are first class entities in _RACR_. They can be queried about the children they induce for nodes of the type they define. It is important to distinguish the names of defined node types from their defining AST rules. Each AST rule precisely defines a node type in the context of a certain _RACR_ specification. Its name however, must be unique only regarding the AST rules of the specification it is part of. It is just a _Scheme_ symbol without any meaning outside the scope of some _RACR_ specification.

In terms of object-oriented programming, every node type corresponds to a class; its children are fields. In CFG terms, it corresponds to a production; its name is the left-hand non-terminal and its children are the right-hand symbols. However, in opposite to CFGs, where several productions can be given for a non-terminal, the node types of a _RACR_ specification must be unique (i.e., must have different names). To simulate alternative productions, node type inheritance can be used.

_RACR_ supports two special node types besides user specified ones: list nodes and bud nodes. Bud nodes are used to represent still missing AST parts. Whenever a node of some type is expected, a bud node can be used instead. They are typically used to decompose and reuse decomposed AST fragments using rewrites. List nodes are used to represent unbounded repetitions. If a child of type `T` with name `c` of a node type `N` is defined to be an unbounded repetition, all `c` children of instances of `N` will be either, a list node with arbitrary many children of type `T` or a bud node. Even if list and bud nodes are non-terminals, their type is undefined. It is not permitted to query such nodes for their type, including sub- and supertype comparisons. And although bud nodes never have children, it is not permitted to query them for children related information (e.g., their number of children).  After all, bud nodes represent still missing, i.e., unspecified, AST parts.

## Specification

### `ast-rule`

```
(ast-rule spec symbol-encoding-rule)
```

Calling this function adds to the given _RACR_ specification the AST rule encoded in the given symbol. To this end, the symbol is parsed. The function aborts with an exception, if the symbol encodes no valid AST rule, there already exists a definition for the l-hand of the rule or the specification is not in the [AST specification phase](architecture.md#api). The grammar used to encode AST rules in symbols is (note, that the grammar has no whitespace):

```
Rule ::= NonTerminal [":" NonTerminal] "->"  [ProductionElement {"-" ProductionElement}];
ProductionElement := NonTerminal ["*"] ["<" ContextName] | Terminal;
NonTerminal ::= UppercaseLetter {Letter} {Number};
Terminal ::= LowercaseLetter {LowercaseLetter} {Number};
ContextName ::= Letter {Letter} {Number};
Letter ::= LowercaseLetter | UppercaseLetter;
LowercaseLetter ::= "a" | "b" | ... | "z";
UppercaseLetter ::= "A" | "B" | ... | "Z";
Number ::= "0" | "1" | ... | "9";
```

Every AST rule starts with a non-terminal (the l-hand), followed by an optional supertype and the actual r-hand consisting of arbitrary many non-terminals and terminals. Every non-terminal of the r-hand can be followed by an optional _Kleene star_, denoting an unbounded repetition (i.e., a list with arbitrary many nodes of the respective non-terminal). Further, r-hand non-terminals can have an explicit context name. Context names can be used to select the respective child for example in attribute definitions (`specify-attribute`, `ag-rule`) or AST traversals (e.g., `ast-child` or `ast-sibling`). If no explicit context name is given, the non-terminal type and optional _Kleene star_ are the respective context name. E.g., for a list of non-terminals of type `N` without explicit context name the context name is `'N*`. For terminals, explicit context names are not permitted. Their name also always is their context name. For every AST rule the context names of its children (including inherited ones) must be unique. Otherwise a later [compilation of the AST specification](abstract-syntax-trees.md#compile-ast-specifications) will throw an exception.

**Note:** _AST rules, and in particular AST rule inheritance, are object-oriented concepts. The l-hand is the class defined by a rule (i.e., a node type) and the r-hand symbols are its fields, each named like the context name of the respective symbol. Compared to common object-oriented languages however, r-hand symbols, including inherited ones, are ordered and represent compositions rather than arbitrary relations, such that it is valid to index them and call them child. The order of children is the order of the respective r-hand symbols and, in case of inheritance, "inherited r-hand first"._

```
(ast-rule spec 'N->A-terminal-A*)
(ast-rule spec 'Na:N->A<A2-A<A3) ; Context-names 4'th & 5'th child: A2 and A3
(ast-rule spec 'Nb:N->)
(ast-rule spec 'Procedure->name-Declaration*<Parameters-Block<Body)
```

### `compile-ast-specifications`

```
(compile-ast-specifications spec start-symbol)
```

Calling this function finishes the [AST specification phase](architecture.md#api) of the given _RACR_ specification, whereby the given symbol becomes the start symbol. The AST specification is checked for completeness and correctness, i.e., (1) all non-terminals are defined, (2) rule inheritance is cycle-free, (3) the start symbol is defined and (4) all non-terminals are reachable and (5) productive. Further, it is ensured, that (5) for every rule the context names of its children are unique. In case of any violation, an exception is thrown. An exception is also thrown, if the given specification is not in the AST specification phase. After executing `compile-ast-specifications` the given specification is in the AG specification phase, such that attributes now can be defined using `specify-attribute` and `ag-rule`.

## Construction

### `create-ast`

```
(create-ast spec non-terminal list-of-children)
```

Function for the construction of non-terminal nodes. Given a _RACR_ specification, the name of a non-terminal to construct (i.e., an AST rule to apply) and a list of children, the function constructs and returns a parentless AST node (i.e., a root) whose type and children are the given ones. Thereby, it is checked, that (1) the given children are of the correct type for the fragment to construct, (2) enough and not to many children are given, (3) every child is a root (i.e., the children do not already belong to/are not already part of another AST) and (4) no attributes of any of the children are in evaluation. In case of any violation an exception is thrown.

**Note:** _Returned fragments do not use the `list-of-children` argument to administer their actual children. Thus, any change to the given list of children (e.g., using `set-car!` or `set-cdr!`) after applying `create-ast` does not change the children of the constructed fragment._

```
(create-ast
 spec
 'N
 ; List of children:
 (list
  ...
  ; For non-terminal children an AST node is expected:
  (create-ast ...)
  ...
  ; For terminals, not an AST node, but their value is expected:
  "value for a terminal"
  ...
  ; For non-terminal children with unbounded cardinality (Kleene closure)
  ; a list node containing their elements is expected:
  (create-ast-list ...)
  ...))
```

### `create-ast-list`

```
(create-ast-list list-of-children)
```

Given a list `l` of non-terminal nodes that are not AST list-nodes construct an AST list-node whose elements are the elements of `l`. An exception is thrown, if an element of `l` is not an AST node, is a list node, already belongs to another AST or has attributes in evaluation.

**Note:** _Returned list-nodes do not use the `list-of-children` argument to administer their actual children. Thus, any change to the given list of children (e.g., using `set-car!` or `set-cdr!`) after applying `create-ast-list` does not change the children of the constructed list-node._

**Note:** _It is not possible to construct AST list nodes containing terminal nodes. Instead however, terminals can be ordinary Scheme lists, such that there is no need for special AST terminal lists._

### `create-ast-bud`

```
(create-ast-bud)
```

Construct a new AST bud-node, that can be used as placeholder within an AST fragment to designate a subtree still to provide. Bud-nodes are valid substitutions for any kind of expected non-terminal child, i.e., whenever a non-terminal node of some type is expected, a bud node can be used instead (e.g., when constructing AST fragments via `create-ast` or `create-ast-list` or when adding another element to a list-node via `rewrite-add`). Since bud-nodes are placeholders, any query for non-terminal node specific information of a bud-node throws an exception (e.g., bud-nodes have no type or attributes and their number of children is not specified etc.).

**Note:** _There exist two main use cases for incomplete ASTs which have "holes" within their subtrees that denote places where appropriate replacements still have to be provided: (1) when constructing ASTs but required parts are not yet known and (2) for the deconstruction and reuse of existing subtrees, i.e., to remove AST parts such that they can be reused for insertion into other places and ASTs. The later use case can be generalised as the reuse of AST fragments within rewrites. The idea thereby is, to use `rewrite-subtree` to insert bud-nodes and extract the subtree replaced._

## Traversal

### `ast-parent`

```
(ast-parent n)
```

Given a node, return its parent if it has any, otherwise thrown an exception.

### `ast-child`

```
(ast-child index-or-context-name n)
```

Given a node, return one of its children selected by context name or child index. If the queried child is a terminal node, not the node itself but its value is returned. An exception is thrown, if the child does not exist.

**Note:** _In opposite to many common programming languages where array or list indices start with `0`, in RACR the index of the first child is `1`, of the second `2` and so on._

**Note:** _Because element nodes within AST list-nodes have no context name, they must be queried by index._

```
(let ((ast
       (with-specification
        (create-specification)
        (ast-rule 'S->A-A*-A<MyContextName)
        (ast-rule 'A->)
        (compile-ast-specifications 'S)
        (compile-ag-specifications)
        (create-ast
         'S
         (list
          (create-ast
           'A
           (list))
          (create-ast-list
           (list))
          (create-ast
           'A
           (list)))))))
  (assert (eq? (ast-child 'A ast) (ast-child 1 ast)))
  (assert (eq? (ast-child 'A* ast) (ast-child 2 ast)))
  (assert (eq? (ast-child 'MyContextName ast) (ast-child 3 ast))))
```

### `ast-sibling`

```
(ast-sibling index-or-context-name n)
```

Given a node `n` which is child of another node `p`, return a certain child `s` of `p` selected by context name or index (thus, `s` is a sibling of `n` or `n`). Similar to `ast-child`, the value of `s`, and not `s` itself, is returned if it is a terminal node. An exception is thrown, if `n` is a root or the sibling does not exist.

### `ast-children`

```
(ast-children n . b1 b2 ... bm)
```

Given a node `n` and arbitrary many child intervals `b1,b2,...,bm` (each a pair consisting of a lower bound `lb` and an upper bound `ub`), return a _Scheme_ list that contains for each child interval `bi = (lb ub)` the children of `n` whose index is within the given interval (i.e., `lb <= child index <= ub`). The elements of the result list are ordered w.r.t. the order of the child intervals `b1,b2,...,bm` and the children of `n`. I.e.:
  * The result lists returned by the child intervals are appended in the order of the intervals.
  * The children of the list computed for a child interval are in increasing index order.

If no child interval is given, a list containing all children of `n` in increasing index order is returned. A child interval with unbounded upper bound (specified using `'*` as upper bound) means "select all children with index `>=` the interval's lower bound". The returned list is a copy -- any change of it (e.g., using `set-car!` or `set-cdr!`) does not change the AST! An exception is thrown, if a child interval queries for a non existent child.

```
(let ((ast
       (with-specification
        (create-specification)
        (ast-rule 'S->t1-t2-t3-t4-t5)
        (compile-ast-specifications 'S)
        (compile-ag-specifications)
        (create-ast 'S (list 1 2 3 4 5)))))
  (assert
   (equal?
    (ast-children ast (cons 2 2) (cons 2 4) (cons 3 '*))
    (list 2 2 3 4 3 4 5)))
  (assert
   (equal?
    (ast-children ast)
    (list 1 2 3 4 5))))
```

### `ast-for-each-child`

```
(ast-for-each-child f n . b1 b2 ... bm)
; f: Processing function of arity two: (1) Index of current child, (2) Current child
; n: Node whose children within the given child intervals will be processed in sequence
; b1 b2 ... bm: Lower-bound/upper-bound pairs (child intervals)
```

Given a function `f`, a node `n` and arbitrary many child intervals `b1,b2,...,bm` (each a pair consisting of a lower bound `lb` and an upper bound `ub`), apply for each child interval `bi = (lb ub)` the function `f` to each child `c` with index `i` with `lb <= i <= ub`, taking into account the order of child intervals and children. Thereby, `f` must be of arity two; Each time `f` is called, its arguments are an index `i` and the respective `i`'th child of `n`. If no child interval is given, `f` is applied to each child once. A child interval with unbounded upper bound (specified using `'*` as upper bound) means "apply `f` to every child with index `>=` the interval's lower bound". An exception is thrown, if a child interval queries for a non existent child.

**Note:** _Like all RACR API functions also `ast-for-each-child` is continuation safe, i.e., it is alright to apply continuations within `f`, such that the execution of `f` is terminated abnormal._

### `ast-find-child`

```
(ast-find-child f n . b1 b2 ... bm)
; f: Search function of arity two: (1) Index of current child, (2) Current child
; n: Node whose children within the given child intervals will be tested in sequence
; b1 b2 ... bm: Lower-bound/upper-bound pairs (child intervals)
```

Given a search function `f`, a node `n` and arbitrary many child intervals `b1, b2,...,bm`, find the first child of `n` within the given intervals which satisfies `f`. Thereby, the children of `n` are tested in the order specified by the child intervals. The search function must accept two parameters -- (1) a child index and (2) the actual child -- and return a truth value telling whether the actual child is the one searched for or not. If no child within the given intervals, which satisfies the search function, exists, `#f` is returned, otherwise the child found. An exception is thrown, if a child interval queries for a non existent child.

**Note:** _The syntax and semantics of child intervals is the one of `ast-for-each-child`, except the search is aborted as soon as a child satisfying the search condition encoded in `f` is found._

```
(let ((ast
       (with-specification
        (create-specification)
        
        ; A program consists of declaration and reference statements:
        (ast-rule 'Program->Statement*)
        (ast-rule 'Statement->)
        ; A declaration declares an entity of a certain name:
        (ast-rule 'Declaration:Statement->name)
        ; A reference refers to an entity of a certain name:
        (ast-rule 'Reference:Statement->name)
        
        (compile-ast-specifications 'Program)
        
        (ag-rule
         lookup
         ((Program Statement*)
          (lambda (n name)
            (ast-find-child
             (lambda (i child)
               (and
                (ast-subtype? child 'Declaration)
                (string=? (ast-child 'name child) name)))
             (ast-parent n)
             ; Child interval enforcing declare before use rule:
             (cons 1 (ast-child-index n))))))
        
        (ag-rule
         correct
         ; A program is correct, if its statements are correct:
         (Program
          (lambda (n)
            (not
             (ast-find-child
              (lambda (i child)
                (not (att-value 'correct child)))
              (ast-child 'Statement* n)))))
         ; A reference is correct, if it is declared:
         (Reference
          (lambda (n)
            (att-value 'lookup n (ast-child 'name n))))
         ; A declaration is correct, if it is no redeclaration:
         (Declaration
          (lambda (n)
            (eq?
             (att-value 'lookup n (ast-child 'name n))
             n))))
        
        (compile-ag-specifications)
        
        (create-ast
         'Program
         (list
          (create-ast-list
           (list
            (create-ast 'Declaration (list "var1"))
            ; First undeclared error:
            (create-ast 'Reference (list "var3"))
            (create-ast 'Declaration (list "var2"))
            (create-ast 'Declaration (list "var3"))
            ; Second undeclared error:
            (create-ast 'Reference (list "undeclared-var")))))))))
  (assert (not (att-value 'correct ast)))
  ; Resolve first undeclared error:
  (rewrite-terminal 'name (ast-child 2 (ast-child 'Statement* ast)) "var1")
  (assert (not (att-value 'correct ast)))
  ; Resolve second undeclared error:
  (rewrite-terminal 'name (ast-child 5 (ast-child 'Statement* ast)) "var2")
  (assert (att-value 'correct ast))
  ; Introduce redeclaration error:
  (rewrite-terminal 'name (ast-child 1 (ast-child 'Statement* ast)) "var2")
  (assert (not (att-value 'correct ast))))
```

### `ast-find-child*`

```
(ast-find-child* f n . b1 b2 ... bm)
; f: Search function of arity two: (1) Index of current child, (2) Current child
; n: Node whose children within the given child intervals will be tested in sequence
; b1 b2 ... bm: Lower-bound/upper-bound pairs (child intervals)
```

Similar to `ast-find-child`, except instead of the first child satisfying `f` the result of `f` for the respective child is returned. If no child satisfies `f`, `#f` is returned.

```
(let ((ast
       (with-specification
        (create-specification)
        (ast-rule 'A->B)
        (ast-rule 'B->t)
        (compile-ast-specifications 'A)
        (compile-ag-specifications)
        (create-ast 'A (list (create-ast 'B (list 1)))))))
  (assert
   (ast-node?
    (ast-find-child ; Return the first child satisfying the search condition
     (lambda (i c)
       (ast-child 't c))
     ast)))
  (assert
   (=
    (ast-find-child* ; Return test result of the first child satisfying the search condition
     (lambda (i c)
       (ast-child 't c))
     ast)
    1)))
```

## Node Information

### `ast-node?`

```
(ast-node? scheme-entity)
```

Given an arbitrary _Scheme_ entity return `#t` if it is an AST node, otherwise `#f`.

### `ast-has-parent?`

```
(ast-has-parent? n)
```

Given a node, return its parent if it has any and `#f` otherwise.

### `ast-child-index`

```
(ast-child-index n)
```

Given a node, return its position within the list of children of its parent. If the node is a root, an exception is thrown.

### `ast-has-child?`

```
(ast-has-child? context-name n)
```

Given a node and context name, return whether the node has a child with the given name or not.

### `ast-num-children`

```
(ast-num-children n)
```

Given a node, return its number of children.

### `ast-has-sibling?`

```
(ast-has-sibling? context-name n)
```

Given a node and context name, return whether the node has a parent node that has a child with the given name or not.

### `ast-node-type`

```
(ast-node-type n)
```

Given a node, return its type, i.e., the non-terminal it is an instance of. If the node is a list or bud node an exception is thrown.

### `ast-node-rule`

```
(ast-node-rule n)
```

Given a node, return the AST rule it represents a derivation of. If the node is a list or bud node an exception is thrown.

### `ast-list-node?`

```
(ast-list-node? n)
```

Given a node, return whether it represents a list of children, i.e., is a list node, or not.

### `ast-bud-node?`

```
(ast-bud-node? n)
```

Given a node, return whether it is a bud node or not.

### `ast-subtype?`

```
(ast-subtype? a1 a2)
```

Given at least one node and another node or non-terminal symbol, return if the first argument is a subtype of the second. The considered subtype relationship is reflexive, i.e., every type is a subtype of itself. An exception is thrown, if non of the arguments is an AST node, any of the arguments is a list- or bud-node or a given non-terminal argument is not defined (the grammar used to decide whether a symbol is a valid non-terminal or not is the one of the node argument).

```
; Let n, n1 and n2 be AST nodes and t a Scheme symbol encoding a non-terminal:
(ast-subtype? n1 n2) ; Is the type of node n1 a subtype of the type of node n2
(ast-subtype? t n) ; Is the type t a subtype of the type of node n
(ast-subtype? n t) ; Is the type of node n a subtype of the type t
```
