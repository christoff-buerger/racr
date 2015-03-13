# Overview #

_RACR_ is a _Scheme_ attribute grammar library providing incremental attribute evaluation in the presence of arbitrary abstract syntax tree rewrites. It provides a set of functions that can be used to specify abstract syntax tree schemes and their attribution and construct respective trees, query their attributes and node information and annotate and rewrite them. Thereby, both, reference attribute grammars and rewriting, are seamlessly integrated, such that rewrites can reuse attributes and attribute values change depending on performed rewrites – a technique we call Reference Attribute Grammar Controlled Rewriting. To reevaluate attributes influenced by abstract syntax tree rewrites, a demand-driven, incremental evaluation strategy, which incorporates the actual execution paths selected at runtime for control-flows within attribute equations, is used. To realise this strategy, a dynamic attribute dependency graph is constructed throughout attribute evaluation – a technique we call Dynamic Attribute Dependency Analyses.

Besides synthesised and inherited attributes, _RACR_ supports reference, parameterised and circular attributes, attribute broadcasting and abstract syntax tree and attribute inheritance. _RACR_ also supports graph pattern matching to ease the specification of complex rewires, whereas patterns can reuse attributes for rewrite conditions such that complex analyses that control rewriting can be specified. Similarly to attribute values, tests for pattern matches are incrementally evaluated and automatically cached. Further, linear pattern matching complexity is guaranteed if involved attributes are already evaluated. Thus, the main drawback of graph rewriting, the matching problem of polynomial complexity for bounded pattern sizes, is attenuated.

Since _RACR_ is an ordinary _Scheme_ library, its functions can arbitrarily interact with _Scheme_ programs and vice versa. To implement attribute equations and rewrites, users reuse or develop ordinary _Scheme_ functions with arbitrary control-flows, function calls, macro expansions and continuations. The required bookkeeping for incremental attribute evaluation is transparently and automatically performed and cannot be bypassed or disturbed.

To enable _RACR_ in embedded scenarios and different technology spaces than _Scheme_, a _C99_ foreign call interface is provided. Using the foreign call interface, _C_ developers can specify their own _RACR_-based languages without much worry about _Scheme_. In particular, users can implement attribute equations as _C_ functions. The caching and incremental evaluation still works. To build a binary _RACR_ implementation including a _Scheme_ R6RS virtual machine with proper garbage collection, that can be linked via the provided headers, _[Racket](http://racket-lang.org)_ is used.

## Features ##

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
  * Automatic caching & demand-driven, incremental evaluation of matches

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
  * Easy packing and unpacking of _Scheme_ values; Automatic marshaling of expected _RACR_ function arguments

## Getting Started ##

To get started just [download and install RACR](Installation.md) and consult the documentation ([html](Documentation.md), [pdf](http://racr.googlecode.com/git/documentation/racr-manual.pdf)). We also provide some [examples](Examples.md).