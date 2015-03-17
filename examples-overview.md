# Examples Overview

In the following we shortly summarize the _RACR_ examples included in the repository. They are contained within subdirectories of the _"examples"_ directory. **Beware:** _RACR_ must be [installed](requirements-and-installation.md) for the examples to work. Examples that are _Scheme_ libraries have to be installed themselves.

## Binary Numbers

**Difficult level:** Attribute grammar and _RACR_ novice
**Size:** Very small
**Scheme library:** No, ordinary _Scheme_ top-level program

_RACR_ introduction based on Knuth's classical attribute grammar paper and its "binary to decimal numbers" example. For details about it see: Donald E. Knuth, _Semantics of Context-Free Languages_, Theory of Computing Systems, volume 2, number 2, pages 127-145, Springer, 1968.

Objectives:
  * Introduction to _RACR_:
    * AST specifications: Non-terminals, productions, inheritance
    * Attribute specifications: Synthesized & inherited attributes
    * AST & attribute access functions

## State Machines

**Difficult level:** Attribute grammar disciple, Reference attribute grammar novice, _RACR_ novice
**Size:** Small
**Scheme library:** No, ordinary _Scheme_ top-level program
**Web documentation:** [Overview and implementation summary](example-state-machines.md)

_RACR_ specification implementing a simple finite state machine language providing attributes to search for certain states by name, to compute the direct successors of a state, the states reachable from it (transitive closure) and if it is a final state. Also the well-formedness of state machines can be checked via attributes. A state machine is well-formed, if, and only if, all states (except the initial state) are reachable from the inital state and from every state (except final states) a final state is reachable.

Objectives:
  * Introduction to reference and circular attributes in _RACR_, their typical applications and advantages
    * Reference attributes: Graph and name analysis problems
    * Circular attributes: Transitive closure, control- and data-flow problems

## Petri Nets

**Difficult level:** Reference attribute grammar disciple, Rewrite disciple, _RACR_ disciple
**Size:** Small/Medium
**Scheme library:** Yes

_RACR_ specification implementing coloured, weighted Petri nets that can be composed using place fusion and support arbitrary input arc conditions and output computations. A reference attribute grammar is used to perform name, enabled, composition and well-formedness analyses of Petri nets and their transitions. _RACR_ rewrites are used to implement their actual execution semantics, i.e., the firing of enabled transitions. A user friendly interface for the specification of Petri nets and their composition and execution is provided in the form of _Scheme_ macros. _RACR_'s incremental attribute evaluation ensures an optimised and efficient enabled analysis, even in case of arbitrarily intertwined compositions, executions (i.e., transition firing) and decompositions. The implemented composition semantics are based on the concept of in- and output ports as presented by Wolfgang Reisig in _Simple Composition of Nets_, Applications and Theory of Petri Nets: 30th International Conference, Lecture Notes in Computer Science, Volume 5606, Pages 23-42, Springer, June 2009.

Objectives:
  * Introduction to reference attribute grammar controlled rewriting & dynamic, incremental reference attribute evaluation:
    * Reference attributes: Name, graph and well-formedness analyses (e.g., the enabled analysis of Petri net transitions)
    * Rewrites: AST represents a state, rewrites are state changes (e.g., rewrites that simulate the firing of transitions by adding and deleting tokens or rewrites that compose Petri nets)
    * Incremental Evaluation: Attributes are only reevaluated if influenced by some rewrite (e.g., the enabled status of transitions is only reevaluated if the last fired transition or the last performed composition influenced it)

## SiPLE (Simple imperative Programming Language Example)

**Difficult level:** Reference attribute grammar experienced, Rewriting novice, _RACR_ experienced
**Size:** Medium
**Scheme library:** Yes

SiPLE is a simple imperative programming language. Its language concepts are:
  * Integer, real and Boolean arithmetics
  * Pointers (including pointers to procedures and pointers)
  * A block-structured name space and nested procedures
  * Lexically-scoped first-class functions (i.e., lexical closures)
  * `While` and `If` control-flow statements
  * Automatic integer to real type coercions (e.g., when assigning an integer value to a variable of type real or when adding an integer to a real value)
  * A save interpreter, that calmly terminates in the presence of errors
SiPLE is strongly typed, such that a static type analysis can be performed.
