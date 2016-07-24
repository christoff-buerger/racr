_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# _RACR_ API Index

## ASTs: Specification

 * [ast-rule](abstract-syntax-trees.md#ast-rule)
 * [compile-ast-specifications](abstract-syntax-trees.md#compile-ast-specifications)

## ASTs: Construction

* [create-ast](abstract-syntax-trees.md#create-ast)
* [create-ast-list](abstract-syntax-trees.md#create-ast-list)
* [create-ast-bud](abstract-syntax-trees.md#create-ast-bud)

## ASTs: Traversal

* [ast-parent](abstract-syntax-trees.md#ast-parent)
* [ast-child](abstract-syntax-trees.md#ast-child)
* [ast-sibling](abstract-syntax-trees.md#ast-sibling)
* [ast-children](abstract-syntax-trees.md#ast-children)
* [ast-for-each-child](abstract-syntax-trees.md#ast-for-each-child)
* [ast-find-child](abstract-syntax-trees.md#ast-find-child)
* [ast-find-child*](abstract-syntax-trees.md#ast-find-child-1)

## ASTs: Node Information

* [ast-node?](abstract-syntax-trees.md#ast-node)
* [ast-has-parent?](abstract-syntax-trees.md#ast-has-parent)
* [ast-child-index](abstract-syntax-trees.md#ast-child-index)
* [ast-has-child?](abstract-syntax-trees.md#ast-has-child)
* [ast-num-children](abstract-syntax-trees.md#ast-num-children)
* [ast-has-sibling?](abstract-syntax-trees.md#ast-has-sibling)
* [ast-node-type](abstract-syntax-trees.md#ast-node-type)
* [ast-node-rule](abstract-syntax-trees.md#ast-node-rule)
* [ast-list-node?](abstract-syntax-trees.md#ast-list-node)
* [ast-bud-node?](abstract-syntax-trees.md#ast-bud-node)
* [ast-subtype?](abstract-syntax-trees.md#ast-subtype)

## Attribution: Specification

* [specify-attribute](attribution.md#specify-attribute)
* [specify-pattern](attribution.md#specify-pattern)
* [ag-rule](attribution.md#ag-rule)
* [compile-ag-specifications](attribution.md#compile-ag-specifications)

## Attribution: Querying

* [att-value](attribution.md#att-value)

## Rewriting: Primitive Rewrite Functions

* [rewrite-terminal](rewriting.md#rewrite-terminal)
* [rewrite-refine](rewriting.md#rewrite-refine)
* [rewrite-abstract](rewriting.md#rewrite-abstract)
* [rewrite-subtree](rewriting.md#rewrite-subtree)
* [rewrite-add](rewriting.md#rewrite-add)
* [rewrite-insert](rewriting.md#rewrite-insert)
* [rewrite-delete](rewriting.md#rewrite-delete)

## Rewriting: Rewrite Strategies

* [perform-rewrites](rewriting.md#perform-rewrites)
* [create-transfomer-for-patter](rewriting.md#create-transformer-for-pattern)

## Annotations: Querying

* [ast-annotation](ast-annotations.md#ast-annotation)
* [undefined-annotation?](ast-annotations.md#undefined-annotation)

## Annotations: Attachment

* [ast-annotation-set!](ast-annotations.md#ast-annotation-set)
* [ast-annotation-remove!](ast-annotations.md#ast-annotation-remove)
* [ast-weave-annotations](ast-annotations.md#ast-weave-annotations)

## Support

* [with-specification](support-api.md#with-specification)
* [with-bindings](support-api.md#with-bindings)
* [specification-&gt;phase](support-api.md#specification-phase)
