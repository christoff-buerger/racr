_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Attribution

_RACR_ supports synthesised and inherited attributes that can be parameterised, circular and references. Attribute definitions are inherited w.r.t. AST inheritance. Thereby, the subtypes of an AST node type can overwrite inherited definitions by providing their own definition. _RACR_ also supports attribute broadcasting, such that there is no need to specify equations that just copy propagate attribute values from parent to child nodes. Some of these features differ from common attribute grammar systems however:

  * **Broadcasting** Inherited _and_ synthesised attributes are broadcasted _on demand_.
  * **Shadowing** Synthesised attribute instances _dynamically_ shadow inherited instances.
  * **AST Fragment Evaluation** Attributes of incomplete ASTs can be evaluated.
  * **Normal Form / AST Query Restrictions** Attribute equations can query AST information without restrictions because of attribute types or contexts.
  * **Completeness** It is not checked if for all attribute contexts a definition exists.

Of course, RACR also differs in its automatic tracking of dynamic attribute dependencies and the incremental attribute evaluation based on it (cf. Chapter 1.1: Efficient Attribute Evaluation). Its differences regarding broadcasting, shadowing, AST fragment evaluation, AST query restrictions and completeness are discussed in the following.

**Broadcasting** If an attribute is queried at some AST node and there exists no definition for the context the node represents, the first successor node with a definition is queried instead. If such a node does not exist a runtime exception is thrown. In opposite to most broadcasting concepts however, _RACR_ makes no difference between synthesised and inherited attributes, i.e., not only inherited attributes are broadcasted, but also synthesised. In combination with the absence of normal form or AST query restrictions, broadcasting of synthesised attributes eases attribute specifications. E.g., if some information has to be broadcasted to `n` children, a synthesised attribute definition computing the information is sufficient. There is no need to specify additional `n` inherited definitions for broadcasting.

**Shadowing** By default, attribute definitions are inherited w.r.t. AST inheritance. If an attribute definition is given for some node type, the definition also holds for all its subtypes. Of course, inherited definitions can be overwritten as used to from object-oriented programming in which case the definitions for subtypes are preferred to inherited ones. Further, the sets of synthesised and inherited attributes are not disjunct. An attribute of a certain name can be synthesised in one context and inherited in another one. If for some attribute instance a synthesised and inherited definition exists, the synthesised is preferred.

**AST Fragment Evaluation** Attribute instances of ASTs that contain bud-nodes or whose root does not represents a derivation w.r.t. the start symbol still can be evaluated if they are well-defined, i.e., do not depend on unspecified AST information. If an attribute instance depends on unspecified AST information, its evaluation throws a runtime exception.

**Normal Form / AST Query Restrictions** A major attribute grammar concept is the local definition of attributes. Given an equation for some attribute and context (i.e., attribute name, node type and children) it must only depend on attributes and AST information provided by the given context. Attribute grammar systems requiring normal form are even more restrictive by enforcing that the defined attributes of a context must only depend on its undefined. In practice, enforcing normal form has turned out to be inconvenient for developers, such that most attribute grammar systems abandoned it. Its main application area is to ease proofs in attribute grammar theories. Also recent research in reference attribute grammars demonstrated, that less restrictive locality requirements can considerably improve attribute grammar development. _RACR_ even goes one step further, by enforcing no restrictions about attribute and AST queries within equations. Developers are free to query ASTs, in particular traverse them, however they like. _RACR's_ leitmotif is, that users are experienced language developers that should not be restricted or patronised. For example, if a developer knows that for some attribute the information required to implement its equation is always located at a certain non-local but relative position from the node the attribute is associated with, he should be able to just retrieve it. And if a software project emphasises a certain architecture, the usage of _RACR_ should not enforce any restrictions, even if "weird" attribute grammar designs may result. There are also theoretic and technical reasons why locality requirements are abandoned. Local dependencies are a prerequisite for static evaluation order and cycle test analyses. With the increasing popularity of demand-driven evaluation, because of much less memory restrictions than twenty years ago, combined with automatic caching and support for circular attributes, the reasons for such restrictions vanish.

**Completeness** Traditionally, attribute grammar systems exploit attribute locality to proof, that for every valid AST all its attribute instances are defined, i.e., an equation is specified for every context. Because of reference attributes and dynamic AST and attribute dispatches, such a static attribute grammar completeness check is impossible for _RACR_. In consequence, it is possible that throughout attribute evaluation an undefined or unknown attribute instance is queried, in which case _RACR_ throws a runtime exception. On the other hand, _RACR_ developers are never confronted with situations where artificial attribute definitions must be given for ASTs that, even they are valid w.r.t. their AST scheme, are never constructed, because of some reason unknown to the attribute grammar system. Such issues are very common, since parsers often only construct a subset of the permitted ASTs. For example, assume an imperative programming language with pointers. In this case, it is much more easy to model the left-hand side of assignments as ordinary expression instead of defining another special AST node type. A check, that left-hands are only dereference expressions or variables, can be realised within the concrete syntax used for parsing. If however, completeness is enforced and some expression that is not a dereference expression or variable has an inherited attribute, the attribute must be defined for the left-hand of assignments, although it will never occur in this context.

## Specification

### `specify-attribute`

```
(specify-attribute spec att-name non-terminal index cached? equation circ-def)
; spec: RACR specification
; att-name: Name of the specified attribute (Scheme symbol).
; non-terminal: AST rule R in whose context the attribute is defined (Scheme symbol).
; index: Index or Scheme symbol representing a context name. Specifies the
;     non-terminal within the context of R for which the definition is.
; cached?: Boolean flag determining, whether the values of instances of
;     the attribute are cached or not.
; equation: Equation used to compute the value of instances of the attribute.
;     Equations have at least one parameter - the node the attribute instance
;     to evaluate is associated with (first parameter).
; circ-def: #f if not circular, otherwise bottom-value/equivalence-function pair
```

Calling this function adds to the given _RACR_ specification the given attribute definition. To this end, it is checked, that the given definition is (1) properly encoded (syntax check), (2) its context is defined, (3) the context is a non-terminal position and (4) the definition is unique (no redefinition error). In case of any violation, an exception is thrown. To specify synthesised attributes the index 0 or the context name '`*` can be used.

**Note:** _There exist only few exceptions when attributes should not be cached. In general, parameterized attributes with parameters whose memoization (i.e., permanent storage in memory) might cause garbage collection problems should never be cached. E.g., when parameters are functions, callers of such attributes often construct the respective arguments -- i.e., functions -- on the fly as anonymous functions. In most Scheme systems every time an anonymous function is constructed it forms a new entity in memory, even if the same function constructing code is consecutively executed. Since attributes are cached w.r.t. their parameters, the cache of such attributes with anonymous function arguments might be cluttered up. If a piece of code constructing an anonymous function and using it as an argument for a cached attribute is executed several times, it might never have a cache hit and always store a cache entry for the function argument/attribute value pair. There is no guarantee that RACR handles this issue, because there is no guaranteed way in Scheme to decide if two anonymous function entities are actually the same function (RACR uses_`equal?`_for parameter comparison). A similar caching issue arises if attribute parameters can be AST nodes. Consider a node that has been argument of an attribute is deleted by a rewrite. Even the node is deleted, it and the AST it spans will still be stored as key in the cache of the attribute. It is only deleted from the cache of the attribute, if the cache of the attribute is flushed because of an AST rewrite influencing its value (including the special case, that the attribute is influenced by the deleted node)._

```
(specify-attribute
 spec
 'att ; Define the attribute att...
 'N   ; in the context of N nodes their...
 'B   ; B child (thus, the attribute is inherited). Further, the attribute is...
 #f   ; not cached,...
 (lambda (n para) ; parameterised (one parameter named para) and...
   ...)
 (cons ; circular.
  bottom-value
  equivalence-function)) ; E.g., equal?
; Meta specification: Specify an attribute using another attribute grammar:
(apply
 specify-attribute
 (att-value 'attribute-computing-attribute-definition meta-compiler-ast))
```

### `specify-pattern`

```
(specify-pattern
 spec ; RACR specification
 att-name ; Name of the specified pattern attribute (a Scheme symbol).
 ; Pattern specification consisting of:
 distinguished-node ; Name of the distinguished node (a Scheme symbol).
 fragments ; List of connected AST fragments reachable from the distinguished node.
 references ; List of references connecting pattern nodes.
 condition?) ; #f or function restricting the applicability of the pattern.
```

Calling this function adds to the given _RACR_ specification an attribute of the given name that can be used to decide if the given pattern matches at the location a queried instance of the attribute is associated with. The attribute's definition context is the type of the distinguished node of the given pattern (cf. below for distinguished node). Attributes defined using `specify-pattern` are called pattern attribute. They are ordinary _RACR_ attributes.

The specified pattern of a pattern attribute consists of arbitrary many AST fragments (`fragments` argument), references between nodes of fragments (`references` argument), a distinguished node that is part of some fragment (`distinguished-node` argument) and a condition (`condition?` argument). Each reference represents a directed edge induced by a parameterless reference attribute. To describe fragments and references, _Scheme_ lists of the following structure are used (`(` and `)` are the ordinary list delimiters and `Scheme-Symbol` and `Scheme-Integer` are arbitrary _Scheme_ symbol and integer values respectively):

```
fragments ::= Fragment*;
Fragment ::= (TypeRestriction NodeName (Node*));
Node ::= (ContextName TypeRestriction NodeName (Node*));
ContextName ::= Scheme-Symbol | Scheme-Integer;
TypeRestriction ::= "#f" | Scheme-Symbol;
NodeName ::= "#f" | Scheme-Symbol;

references ::= (Reference*);
Reference ::= (AttributeName Source Target);
AttributeName ::= Scheme-Symbol;
Source ::= Scheme-Symbol;
Target ::= Scheme-Symbol;
```

The non-terminals of the above grammars have the following semantics regarding the homomorph mapping of pattern nodes and edges to a host graph:
  * **TypeRestriction** The node must be of the given type or a subtype. In case of `#f` it can be of arbitrary type (including list and bud nodes).
  * **NodeName** If the node name is not `#f`, the node `h` of the host graph the respective pattern node is mapped to is bound to the given name. To do so a pair of the form `(node-name host-graph-node)` is constructed. The list of these pairs is called binding list. It is a proper association list and returned if the pattern matches. Node names are also required to specify the distinguished node and references between nodes. The distinguished node of a pattern is the node named liked the given `distinguished-node` argument when `specify-pattern` was called.
  * **ContextName** If the context name is a _Scheme_ symbol `s`, the node must be an `s` child. If the context is an integer `i`, the node must be the `i`'th element of a list.
  * **Node`*`** The children of a node.
  * **AttributeName** Name of the parameterless reference attribute inducing the respective reference edge.
  * **Source** Name of the node the reference edge starts from.
  * **Target** Name of the node the reference edge points to.

Given an attribute instance for a pattern, the pattern does not match at the respective instance, if and only if, it does not match structurally or conditionally. A pattern does not match structurally, if and only if, mapping the distinguished node of the pattern to the node its respective pattern attribute instance is associated with, the rest of the pattern cannot be mapped. A pattern does not match conditionally, if and only if, it does not match structurally or it has a condition which is not satisfied. A given condition is not satisfied if, and only if, it evaluates to `#f` when applied to the binding list constructed throughout structural matching and the arguments given when calling the pattern attribute. If `#f` is given as `condition?` argument, the pattern has no condition, otherwise the given `condition?` argument is used as condition during matching.

If a pattern attribute instance is queried and its pattern does not match at the instance, it returns `#f`, otherwise it returns the binding list constructed throughout structural matching.

Patterns must satisfy certain conditions to be well-formed:
  * A node of the pattern is named like the given `distinguished-node` argument.
  * Node names are unique.
  * The distinguished node has a type restriction which is not a list.
  * If a node of the pattern has a type restriction, the type is defined according to the given _RACR_ specification.
  * For every source and target of a reference there exists an equally named node in the pattern.
  * All nodes of the pattern are reachable from the distinguished node considering bidirectional child/parent edges and directed references.
  * There exists an AST w.r.t. the given _RACR_ specification where the pattern matches only considering its AST fragments.
In case of any violation, `specify-pattern` throws an exception.

**Note:** _The binding list given to a condition for its evaluation is **not** a copy. Conditions can manipulate it and therefore the bindings returned in case the pattern matches. This is considered to be bad style however._

**Note:** _Conditions often depend on nodes bound throughout structural matching. To ease their development, the `with-bindings` form can be used. It provides convenient means to establish bindings for matched nodes without manual searches in binding lists._

**Note:** _Patterns can be used for the convenient specification of complex AST rewrites. The general idea is to specify patterns that only match in situations when to rewrite and bind all the nodes relevant for rewriting. The patterns, in combination with `perform-rewrites` and `create-transformer-for-pattern`, can then be reused to define a set of transformers that rewrite the AST until no further rewrites are possible. This scenario corresponds to the application of a set of rewrite rules on a host graph. The patterns are the l-hands of the rewrite rules, the transformers are their respective r-hands and `perform-rewrites` is their application._

**Note:** _Attributes specified using `specify-pattern` are ordinary non-circular synthesised attributes. They are inherited, shadowed, broadcasted, automatically cached, demand-driven and incrementally evaluated and can reuse other attributes in pattern conditions. They can also be parameterised, in which case their condition is applied to the binding list and the additional arguments, if they have any condition at all._

**Note:** _Assuming the reference edges induced by reference attributes of a pattern are already evaluated, RACR guarantees constant structural matching time for the evaluation of pattern attributes. In consequence, matching is linear in RACR and not polynomial as in general graph rewriting with bounded pattern sizes. To find all matches of a pattern, each node of the host graph has to be visited once and, in case the node is of the type of the pattern's distinguished node, its respective pattern attribute evaluated._

```
(specify-pattern
 some-racr-specification
 some-pattern-attribute-name
 some-distinguished-node-name
 some-ast-fragments
 some-references
 ; Assume the pattern establishes bindings A and B, we can specify
 ; a pattern condition depending on both:
 (with-bindings
  (A B)
  ...)) ; Arbitrary code that somehow uses A and B
```

### `ag-rule`

```
(ag-rule
 attribute-name
 ; Arbitrary many, but at least one, definitions of any of the following forms:
 ((non-terminal context-name) equation) ; Default: cached and non-circular
 ((non-terminal context-name) cached? equation)
 ((non-terminal context-name) equation bottom equivalence-function)
 ((non-terminal context-name) cached? equation bottom equivalence-function)
 (non-terminal equation) ; No context name = synthesized attribute
 (non-terminal cached? equation)
 (non-terminal equation bottom equivalence-function)
 (non-terminal cached? equation bottom equivalence-function))
; attribute-name, non-terminal, context-name: Scheme identifiers, not symbols!
```

Syntax definition which eases the specification of attributes by:
  * Permitting the specification of arbitrary many definitions for a certain attribute for different contexts without the need to repeat the attribute name several times
  * Automatic quoting of attribute names (thus, the given name must be an ordinary identifier)
  * Automatic quoting of non-terminals and context names (thus, contexts must be ordinary identifiers)
  * Optional caching and circularity information (by default caching is enabled and attribute definitions are non-circular)
  * Context names of synthesized attribute definitions can be left

The `ag-rule` form exists only for convenient reasons. All its functionalities can also be achieved using `specify-attribute`.

**Note:** _Sometimes attribute definitions shall be computed by a Scheme function rather than being statically defined. In such cases the `ag-rule` form is not appropriate, because it expects identifiers for the attribute name and contexts. Moreover, the automatic context name quoting prohibits the specification of contexts using child indices. The `specify-attribute` function must be used instead._

### `compile-ag-specifications`

```
(compile-ag-specifications spec)
```

Calling this function finishes the [AG specification phase](architecture.md#api) of the given _RACR_ specification, such that it is now in the evaluation phase where ASTs can be instantiated, evaluated, annotated and rewritten. An exception is thrown, if the given specification is not in the AG specification phase.

## Evaluation and Querying

### `att-value`

```
(att-value attribute-name node . arguments)
```

Given a node, return the value of one of its attribute instances. In case no proper attribute instance is associated with the node itself, the search is extended to find a broadcast solution. If required, the found attribute instance is evaluated, whereupon all its meta-information like dependencies etc. are computed. The function has a variable number of arguments, whereas its optional parameters are the actual arguments for parameterized attributes. An exception is thrown, if the given node is a bud-node, no properly named attribute instance can be found, the wrong number of arguments is given, the attribute instance depends on itself but its definition is not declared to be circular or the attribute equation is erroneous (i.e., its evaluation aborts with an exception).

```
; Let n be an AST node:
(att-value 'att n) ; Query attribute instance of n that represents attribute att
(att-value 'lookup n "myVar") ; Query parameterised attribute with one argument
; Dynamic attribute dispatch:
(att-value
 (att-value 'attribute-computing-attribute-name n)
 (att-value 'reference-attribute-computing-AST-node n))
```
