# API Index

## ASTs: Specification

 * [ast-rule](../wiki/ASTs.md#ast-rule)
 * [compile-ast-specifications](ASTs#compile-ast-specifications.md)

## ASTs: Construction

* [create-ast](ASTs#create-ast.md)
* [create-ast-list](ASTs#create-ast-list.md)
* [create-ast-bud](ASTs#create-ast-bud.md)

## ASTs: Traversal

* [ast-parent](ASTs#ast-parent.md)
* [ast-child](ASTs#ast-child.md)
* [ast-sibling](ASTs#ast-sibling.md)
* [ast-children](ASTs#ast-children.md)
* [ast-for-each-child](ASTs#ast-for-each-child.md)
* [ast-find-child](ASTs#ast-find-child.md)
* [ast-find-child*](ASTs#ast-find-child*.md)

## ASTs: Node Information

* [ast-node?](ASTs#ast-node?.md)
* [ast-has-parent?](ASTs#ast-has-parent?.md)
* [ast-child-index](ASTs#ast-child-index.md)
* [ast-has-child?](ASTs#ast-has-child?.md)
* [ast-num-children](ASTs#ast-num-children.md)
* [ast-has-sibling?](ASTs#ast-has-sibling?.md)
* [ast-node-type](ASTs#ast-node-type.md)
* [ast-node-rule](ASTs#ast-node-rule.md)
* [ast-list-node?](ASTs#ast-list-node?.md)
* [ast-bud-node?](ASTs#ast-bud-node?.md)
* [ast-subtype?](ASTs#ast-subtype?.md)

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
