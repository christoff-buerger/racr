# API Index

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

* [rewrite-terminal](Rewrites#rewrite-terminal.md)
* [rewrite-refine](Rewrites#rewrite-refine.md)
* [rewrite-abstract](Rewrites#rewrite-abstract.md)
* [rewrite-subtree](Rewrites#rewrite-subtree.md)
* [rewrite-add](Rewrites#rewrite-add.md)
* [rewrite-insert](Rewrites#rewrite-insert.md)
* [rewrite-delete](Rewrites#rewrite-delete.md)

## Rewriting: Rewrite Strategies

* [perform-rewrites](Rewrites#perform-rewrites.md)
* [create-transfomer-for-patter](Rewrites#create-transformer-for-pattern.md)

## Annotations: Attachment

* [ast-annotation-set!](Annotations#ast-annotation-set!.md)
* [ast-weave-annotations](Annotations#ast-weave-annotations.md)
* [ast-annotation-remove!](Annotations#ast-annotation-remove!.md)

## Annotations: Querying

* [ast-annotation?](Annotations#ast-annotation?.md)
* [ast-annotation](Annotations#ast-annotation.md)

## Support

* [with-specification](SupportAPI#with-specification.md)
* [with-bindings](SupportAPI#with-bindings.md)
* [specification-&gt;phase](SupportAPI#specification->phase.md)
