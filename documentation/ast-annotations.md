_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# AST Annotations

Often, additional information or functionalities, which can arbitrarily change or whose value and behaviour depends on time, have to be supported by ASTs. Examples are special node markers denoting certain imperative actions or stateful functions for certain AST nodes. Attributes are not appropriate in such cases, since their intension is to be side-effect free, such that their value does not depend on their query order or if they are cached. Further, it is not possible to arbitrarily attach attributes to ASTs. Equal contexts will always use equal attribute definitions for their attribute instances. To realise stateful or side-effect causing node dependent functionalities, the annotation API of _RACR_ can be used. AST annotations are named entities associated with AST nodes that can be arbitrarily attached, detached, changed and queried. Thereby, annotation names are ordinary _Scheme_ symbols and their values are arbitrary _Scheme_ entities. However, to protect users against misuse, _RACR_ does not permit, throughout the evaluation of an attribute, the application of any annotation functionalities on (other) nodes within the same AST the attribute is associated with.

## Attachment

### `ast-annotation-set!`

```
(ast-annotation-set! n a v)
```

Given a node `n`, a _Scheme_ symbol `a` representing an annotation name and an arbitrary value `v`, add an annotation with name `a` and value `v` to `n`. If `n` already has an annotation named `a`, set its value to `v`. If `v` is a function, the value of the annotation is a function calling `v` with the node the annotation is associated with (i.e., `n`) as first argument and arbitrary many further given arguments. An exception is thrown if any attributes of the AST `n` is part of are in evaluation.

**Note:** _Since terminal nodes as such cannot be retrieved (cf. `ast-child`), but only their value, the annotation of terminal nodes is not possible._

```
(let ((n (function-returning-an-ast)))
  ; Attach annotations:
  (ast-annotation-set! n 'integer-value 3)
  (ast-annotation-set!
   n
   'function-value
   (lambda (associated-node integer-argument)
     integer-argument))
  ; Query annotations:
  (assert
   (=
    (ast-annotation n 'integer-value)
    ; Apply the value of the 'function-value annotation. Note, that
    ; the returned function has one parameter (integer-argument). The
    ; associated-node parameter is automatically bound to n:
    ((ast-annotation n 'function-value) 3))))
```

### `ast-weave-annotations`

```
(ast-weave-annotations n t a v)
```

Given a node `n` spanning an arbitrary AST fragment, a node type `t` and an annotation name `a` and value `v`, add to each node of type `t` of the fragment, which does not yet have an equally named annotation, the given annotation using `ast-annotation-set!`. An exception is thrown, if any attributes of the AST `n` is part of are in evaluation.

**Note:** _To annotate all list- or bud-nodes within ASTs,_`'list-node`_or_`'bud-node`_can be used as node type_`t`_respectively._

### `ast-annotation-remove!`

```
(ast-annotation-remove! n a)
```

Given a node `n` and an annotation name `a`, remove any equally named annotation associated with `n`. An exception is thrown, if any attributes of the AST `n` is part of are in evaluation.

## Querying

### `ast-annotation?`

```
(ast-annotation? n a)
```

Given a node `n` and an annotation name `a`, return whether `n` has an annotation with name `a` or not. An exception is thrown, if any attributes of the AST `n` is part of are in evaluation.

### `ast-annotation`

```
(ast-annotation n a)
```

Given a node `n` and an annotation name `a`, return the value of the respective annotation of `n` (i.e., the value of the annotation with name `a` that is associated with the node `n`). An exception is thrown, if `n` has no such annotation or any attributes of the AST it is part of are in evaluation.
