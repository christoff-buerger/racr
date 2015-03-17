# API Index

## ASTs: Specification

 * [ast-rule](abstract-syntax-trees.md#ast-rule)
 * [compile-ast-specifications](abstract-syntax-trees#compile-ast-specifications.md)

## ASTs: Construction

* [create-ast](abstract-syntax-trees#create-ast.md)
* [create-ast-list](abstract-syntax-trees#create-ast-list.md)
* [create-ast-bud](abstract-syntax-trees#create-ast-bud.md)

## ASTs: Traversal

* [ast-parent](abstract-syntax-trees#ast-parent.md)
* [ast-child](abstract-syntax-trees#ast-child.md)
* [ast-sibling](abstract-syntax-trees#ast-sibling.md)
* [ast-children](abstract-syntax-trees#ast-children.md)
* [ast-for-each-child](abstract-syntax-trees#ast-for-each-child.md)
* [ast-find-child](abstract-syntax-trees#ast-find-child.md)
* [ast-find-child*](abstract-syntax-trees#ast-find-child*.md)

## ASTs: Node Information

* [ast-node?](abstract-syntax-trees#ast-node?.md)
* [ast-has-parent?](abstract-syntax-trees#ast-has-parent?.md)
* [ast-child-index](abstract-syntax-trees#ast-child-index.md)
* [ast-has-child?](abstract-syntax-trees#ast-has-child?.md)
* [ast-num-children](abstract-syntax-trees#ast-num-children.md)
* [ast-has-sibling?](abstract-syntax-trees#ast-has-sibling?.md)
* [ast-node-type](abstract-syntax-trees#ast-node-type.md)
* [ast-node-rule](abstract-syntax-trees#ast-node-rule.md)
* [ast-list-node?](abstract-syntax-trees#ast-list-node?.md)
* [ast-bud-node?](abstract-syntax-trees#ast-bud-node?.md)
* [ast-subtype?](abstract-syntax-trees#ast-subtype?.md)

## Attribution: Specification

* [specify-attribute](Attributes#specify-attribute.md)
* [specify-pattern](Attributes#specify-pattern.md)
* [ag-rule](Attributes#ag-rule.md)
* [compile-ag-specifications](Attributes#compile-ag-specifications.md)

## Attribution: Querying

* [att-value](Attributes#att-value.md)

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
