# _RACR_ Reference Manual: A _Scheme_ Library for Reference Attribute Grammar Controlled Rewriting

## Abstract

_RACR_ supports incremental attribute evaluation in the presence of arbitrary abstract syntax tree rewrites. It provides a set of functions that can be used to specify abstract syntax tree schemes and their attribution and construct respective trees, query their attributes and node information and annotate and rewrite them. Thereby, both, reference attribute grammars and rewriting, are seamlessly integrated, such that rewrites can reuse attributes and attribute values change depending on performed rewrites -– a technique we call Reference Attribute Grammar Controlled Rewriting. To reevaluate attributes influenced by abstract syntax tree rewrites, a demand-driven, incremental evaluation strategy, which incorporates the actual execution paths selected at runtime for control-flows within attribute equations, is used. To realise this strategy, a dynamic attribute dependency graph is constructed throughout attribute evaluation –- a technique we call Dynamic Attribute Dependency Analyses.

Besides synthesised and inherited attributes, _RACR_ supports reference, parameterised and circular attributes, attribute broadcasting and abstract syntax tree and attribute inheritance. _RACR_ also supports graph pattern matching to ease the specification of complex rewires, whereas patterns can reuse attributes for rewrite conditions such that complex analyses that control rewriting can be specified. Similarly to attribute values, tests for pattern matches are incrementally evaluated and automatically cached. Further, linear pattern matching complexity is guaranteed if involved attributes are already evaluated. Thus, the main drawback of graph rewriting, the matching problem of polynomial complexity for bounded pattern sizes, is attenuated.

Since _RACR_ is an ordinary _Scheme_ library, its functions can arbitrarily interact with _Scheme_ programs and vice versa. To implement attribute equations and rewrites, users reuse or develop ordinary _Scheme_ functions with arbitrary control-flows, function calls, macro expansions and continuations. The required bookkeeping for incremental attribute evaluation is transparently and automatically performed by _RACR_ and cannot be bypassed or disturbed.

The reference manual documents _RACR's_ features, instantiation and usage. It summarises and exemplifies _RACR's_
application programming interface. The Implementation of reference attribute grammar controlled rewriting or dynamic attribute dependency analyses is not discussed. Readers should be comfortable with _Scheme_; This manual is not a _Scheme_ introduction!

## Features

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
  * Easy packing and unpacking of _Scheme_ values; Automatic marshalling of expected _RACR_ function arguments

## Getting Started

To get started just [download and install _RACR_](requirements-and-installation.md) and consult the [documentation](contents.md). We also provide some [examples](../examples/examples-overview.md).
