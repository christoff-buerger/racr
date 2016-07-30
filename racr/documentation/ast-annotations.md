_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# AST Annotations

Often, additional information or functionalities, which can arbitrarily change or whose value and behaviour depends on time, have to be supported by ASTs. Examples are special node markers denoting certain imperative actions or stateful functions for certain AST nodes. Attributes are not appropriate in such cases, since their intension is to be side-effect free, such that their value does not depend on their query order or if they are cached. Further, it is not possible to arbitrarily attach attributes to ASTs. Equal contexts will always use equal attribute definitions for their attribute instances. To realise stateful or side-effect causing node dependent functionalities, the annotation API of _RACR_ can be used. AST annotations are named entities associated with AST nodes that can be arbitrarily attached, detached, changed and queried. Thereby, annotation names are ordinary _Scheme_ symbols and their values are arbitrary _Scheme_ entities. However, to protect users against misuse, _RACR_ does not permit the usage of annotation facilities throughout attribute evaluation.

## Querying

### `ast-annotation`

```
(ast-annotation n a)
```

Given an AST node `n` and an annotation name `a`, return the value of the respective annotation of `n` (i.e., the value of the annotation with name `a` that is associated with node `n`). If the annotation is not present at `n`, a special undefined annotation entity is returned. To check if the annotation was defined, the `undefined-annotation?` function can be applied on the returned value. An exception is thrown, if any attributes of the AST `n` is part of are in evaluation.

### `undefined-annotation?`

```
(undefined-annotation? v)
```

Check if an arbitrary _Scheme_ entity `v` is the undefined annotation entity; return `#t` if it is, otherwise `#f`.

**Note:** `undefined-annotation?` _can be used to decide if an annotation is/was present and therefore its value was returned by_ `ast-annotation` _or_ `ast-annotation-remove!`_._

## Attachment

### `ast-annotation-set!`

```
(ast-annotation-set! n a v)
```

Given an AST node `n`, a _Scheme_ symbol `a` representing an annotation name and an arbitrary value `v`, add an annotation with name `a` and value `v` to `n`. If `n` already has an annotation named `a`, set its value to `v`. An exception is thrown if any attributes of the AST `n` is part of are in evaluation, `v` is the undefined annotation entity or `a` is not a _Scheme_ symbol.

**Note:** _Since terminal nodes as such cannot be retrieved (cf. `ast-child`), but only their value, the annotation of terminal nodes is not possible._

### `ast-annotation-remove!`

```
(ast-annotation-remove! n a)
```

Given an AST node `n` and an annotation name `a`, remove any equally named annotation associated with `n` and return its value. If no respective annotation was removed, the undefined annotation entity is returned. To check if the annotation was defined, the `undefined-annotation?` function can be applied on the returned value. An exception is thrown, if any attributes of the AST `n` is part of are in evaluation.

### `ast-weave-annotations`

**Warning:** _Deprecated functionality; will be removed in future versions!_

```
(ast-weave-annotations n t a v)
```

Given an AST node `n` spanning an arbitrary AST fragment, a node type `t` and an annotation name `a` and value `v`, add to each node of type `t` of the fragment, which does not yet have an equally named annotation, the given annotation using `ast-annotation-set!`. Exceptions are thrown according to `ast-annotation-set!`.

**Note:** _To annotate all list- or bud-nodes within ASTs,_`'list-node` _or_ `'bud-node` _can be used as node type_ `t` _respectively._
