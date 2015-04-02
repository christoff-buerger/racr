_[>> Abstract <<](abstract.md) [>> Features <<](features.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# _RACR_ Features

Abstract Syntax Trees (AST):
  * Typed non-terminals
  * _EBNF_ (Kleene closure & list-nodes)
  * Non-terminal inheritance
  * AST fragments (incomplete ASTs containing special non-terminal markers called bud-nodes which designate places where proper subtrees still have to be provided)
  * AST annotations (weaving of arbitrary entities into ASTs)

Attribute Grammars:
  * Synthesised and inherited attributes
  * Circular attributes
  * Reference attributes
  * Parameterised attributes
  * Attribute broadcasting
  * Attribute inheritance & shadowing
  * Automatic attribute caching & demand-driven, incremental attribute evaluation

Graph Patterns:
  * Patterns consisting of arbitrary many AST fragments and connecting reference edges
  * Reuse attributes to specify complex rewrite conditions
  * Linear matching complexity
  * Automatic caching & demand-driven, incremental evaluation of pattern matching

AST Rewrites:
  * Subtree and terminal replacement
  * Non-terminal refinement and abstraction
  * Insertion, deletion and addition of list elements
  * Deconstruction, refinement and reuse of AST fragments via bud-node replacement
  * Automatic attribute dependency and cache maintenance in the presence of rewrites

_C_ Foreign Call Interface:
  * Use complete _RACR_ API in _C_ programs
  * Dynamically load _RACR_ programs written in _Scheme_
  * Use _C_ functions to implement attribute equations
  * Easy packing and unpacking of _Scheme_ values; Automatic marshalling of expected _RACR_ function arguments
