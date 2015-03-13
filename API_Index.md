# API Index #

## ASTs: Specification ##

[ast-rule](ASTs#ast-rule.md)<br>
<a href='ASTs#compile-ast-specifications.md'>compile-ast-specifications</a>

<h2>ASTs: Construction</h2>

<a href='ASTs#create-ast.md'>create-ast</a><br>
<a href='ASTs#create-ast-list.md'>create-ast-list</a><br>
<a href='ASTs#create-ast-bud.md'>create-ast-bud</a><br>

<h2>ASTs: Traversal</h2>

<a href='ASTs#ast-parent.md'>ast-parent</a><br>
<a href='ASTs#ast-child.md'>ast-child</a><br>
<a href='ASTs#ast-sibling.md'>ast-sibling</a><br>
<a href='ASTs#ast-children.md'>ast-children</a><br>
<a href='ASTs#ast-for-each-child.md'>ast-for-each-child</a><br>
<a href='ASTs#ast-find-child.md'>ast-find-child</a><br>
<a href='ASTs#ast-find-child*.md'>ast-find-child*</a><br>

<h2>ASTs: Node Information</h2>

<a href='ASTs#ast-node?.md'>ast-node?</a><br>
<a href='ASTs#ast-has-parent?.md'>ast-has-parent?</a><br>
<a href='ASTs#ast-child-index.md'>ast-child-index</a><br>
<a href='ASTs#ast-has-child?.md'>ast-has-child?</a><br>
<a href='ASTs#ast-num-children.md'>ast-num-children</a><br>
<a href='ASTs#ast-has-sibling?.md'>ast-has-sibling?</a><br>
<a href='ASTs#ast-node-type.md'>ast-node-type</a><br>
<a href='ASTs#ast-node-rule.md'>ast-node-rule</a><br>
<a href='ASTs#ast-list-node?.md'>ast-list-node?</a><br>
<a href='ASTs#ast-bud-node?.md'>ast-bud-node?</a><br>
<a href='ASTs#ast-subtype?.md'>ast-subtype?</a>

<h2>Attribution: Specification</h2>

<a href='Attributes#specify-attribute.md'>specify-attribute</a><br>
<a href='Attributes#specify-pattern.md'>specify-pattern</a><br>
<a href='Attributes#ag-rule.md'>ag-rule</a><br>
<a href='Attributes#compile-ag-specifications.md'>compile-ag-specifications</a>

<h2>Attribution: Querying</h2>

<a href='Attributes#att-value.md'>att-value</a>

<h2>Rewriting: Primitive Rewrite Functions</h2>

<a href='Rewrites#rewrite-terminal.md'>rewrite-terminal</a><br>
<a href='Rewrites#rewrite-refine.md'>rewrite-refine</a><br>
<a href='Rewrites#rewrite-abstract.md'>rewrite-abstract</a><br>
<a href='Rewrites#rewrite-subtree.md'>rewrite-subtree</a><br>
<a href='Rewrites#rewrite-add.md'>rewrite-add</a><br>
<a href='Rewrites#rewrite-insert.md'>rewrite-insert</a><br>
<a href='Rewrites#rewrite-delete.md'>rewrite-delete</a>

<h2>Rewriting: Rewrite Strategies</h2>

<a href='Rewrites#perform-rewrites.md'>perform-rewrites</a><br>
<a href='Rewrites#create-transformer-for-pattern.md'>create-transfomer-for-patter</a>

<h2>Annotations: Attachment</h2>

<a href='Annotations#ast-annotation-set!.md'>ast-annotation-set!</a><br>
<a href='Annotations#ast-weave-annotations.md'>ast-weave-annotations</a><br>
<a href='Annotations#ast-annotation-remove!.md'>ast-annotation-remove!</a>

<h2>Annotations: Querying</h2>

<a href='Annotations#ast-annotation?.md'>ast-annotation?</a><br>
<a href='Annotations#ast-annotation.md'>ast-annotation</a>

<h2>Support</h2>

<a href='SupportAPI#with-specification.md'>with-specification</a><br>
<a href='SupportAPI#with-bindings.md'>with-bindings</a><br>
<a href='SupportAPI#specification->phase.md'>specification-&gt;phase</a>