# Examples Overview

**Beware:** _RACR_ must be [installed](../racr/documentation/requirements-and-installation.md) for the examples to work.

**Note:** The examples are ordered in increasing difficulty.

## Binary Numbers

**Difficult level:** Attribute grammar novice, _RACR_ novice
**New features:** Abstract syntax trees and attribution, synthesised and inherited attributes
**Size:** Very small
**Scheme library:** No, ordinary _Scheme_ top-level program

Introduction of _RACR_ basics based on Knuth's classical attribute grammar paper and its "binary to decimal numbers" example:

> Donald Ervin Knuth. "Semantics of Context-Free Languages".
> In: _Theory of Computing Systems_.
> Volume 2, Number 2. Springer, June 1968, pages 127-145.

**Objective:** Introduction to attribute grammars in _RACR_. Presentation of _RACR's_ basic application programming interface.

 * Abstract syntax tree schemes: non-terminals, productions, inheritance
 * Abstract syntax tree attributions: synthesised and inherited attributes
 * Abstract syntax tree and attribute query functions

## State Machines

**Difficult level:** Reference attribute grammar novice, _RACR_ novice
**New features:** Reference and circular attributes, attribute broadcasting
**Size:** Small
**Scheme library:** No, ordinary _Scheme_ top-level program

Simple language for the specification of finite state machines. The implementation provides attributes to search for certain states by name, to compute the direct successors of a state and the states reachable from it (transitive closure). Also the well-formedness of state machines can be checked via attributes. A state machine is well-formed, if its states are unique, the initial state is defined, the source and target states of transitions are defined and all states (except the initial state) are reachable from the initial state and from every state (except final states) a final state is reachable.

The example shows, how reference attributes declaratively induce edges on abstract syntax trees, extending them to abstract syntax graphs. Such reference attribute induced edges form semantic overlay graphs and are typical for name and type analyses. In the example, reference attributes are used to 'transform' the abstract syntax tree, which was constructed by parsing a textual state machine representation, to the respective, actual, state machine diagram. The graph resulting from this name analysis is well-suited for further analyses, like the implemented state reachability.

**Objective:** Introduction to reference and circular attributes, their typical applications and advantages.

  * Reference attributes: graph analyses (like a name analyses to lookup the incoming and outgoing transitions of states)
  * Circular attributes: transitive closure analyses (like state reachability)

**Documentation:** [Implementation summary](state-machines/documentation/state-machines.md)

## _SLE 2015_: RAG-controlled rewriting motivation and overview

**Difficult level:** RAG-controlled rewriting novice, _RACR_ novice
**New features:** Rewrites
**Size:** Small
**Scheme library:** No, ordinary _Scheme_ top-level program
**Peer-reviewed:** Published at the [_8th ACM SIGPLAN International Conference on Software Language Engineering_](http://www.sleconf.org/2015/)

This example summarises and motivates the essential features of _RACR_ by implementing a simple nested programming language, enriched with type coercion, superfluous cast optimisation and type refactoring. The example gives an overview of the basic concepts of RAG-controlled rewriting:

  * How reference attributes extend _abstract syntax trees_ to _abstract syntax graphs_
  * How _dynamic attribute dependencies_ extend abstract syntax graphs to _dynamic attribute dependency graphs_
  * The benefits of dynamic dependencies for _incremental attribute evaluation_
  * How reference attribute grammar based _analyses_ can be used to ease the development of rewrite-based _transformations_

The example is accompanied by a peer-reviewed paper which motivates and explains these concepts:

> Christoff Bürger.
> "Reference Attribute Grammar Controlled Graph Rewriting: Motivation and Overview".
> In: _Proceedings of the 2015 ACM SIGPLAN International Conference on Software Language Engineering_.
> ACM, October 2015, pages 89-100.
> [Paper at the ACM Digital Library](http://dl.acm.org/citation.cfm?id=2814257)

**Objective:** Self-contained, easy and fast to understand introduction to RAG-controlled rewriting; overview of essential _RACR_ features.

 * Abstract syntax graph schemes and respective query and rewrite functions
 * Abstract syntax graph memoization
 * Application of attribute-based analyses for rewrite-based transformations
 * Incremental evaluation

## Atomic Petri Nets

**Difficult level:** RAG-controlled rewriting novice, _RACR_ disciple
**New application:** RAG-controlled rewriting for incremental program execution
**Size:** Small
**Scheme library:** Yes

Implementation of a coloured, weighted Petri net interpreter that supports arbitrary input arc conditions and output computations. Reference attribute grammar based analyses are used to perform name, enabled and well-formedness analyses of Petri nets; rewrites are used to implement their actual execution semantics, i.e., the firing of enabled transitions.

The example demonstrates the reuse of attribute-based analyses to ease and control rewriting. The presented enabled analysis deduces all enabled transitions and their respective consumed tokens. Given this information, execution via rewriting is straight forward: as long as there are enabled transitions, select one, delete its consumed tokens and add the produced ones. During this process, enabled analysis is automatically optimised by incremental attribute evaluation.

**Objective:** Validation of the benefits of _RACR's_ incremental evaluation semantics in an intuitively comprehensible scenario with interactive, mutual-dependent analyses and transformations (execution semantics of Petri nets).

 * Attribute-based analyses: name, graph and well-formedness analyses (e.g., the enabled analysis of Petri net transitions)
 * Rewrite-based transformations: Abstract syntax tree represents a state, rewrites are state changes (e.g., rewrites that delete consumed and add produced tokens of fired transitions)
 * Mutual-dependent, incremental evaluation of analyses and transformations: attributes are only re-evaluated if they depend on information changed by rewrite applications (e.g., the enabled status of transitions is only re-evaluated if the last fired transition influenced it) and transformations are derived by attributes (firing semantics defined by means of enabled analysis)

## _Language Workbench Challenges 2013 & 2014_: Questionnaires

**Difficult level:** RAG-controlled rewriting disciple, _RACR_ experienced
**New application:** RAG-controlled rewriting for interactive runtime models
**Size:** Small/Medium
**Scheme library:** Yes

Implementation of the Questionnaire Language, the competition scenario of the [_Language Workbench Challenges 2013 and 2014_](https://2016.splashcon.org/track/lwc2016). The Questionnaire Language provides means for the convenient specification of formulas, consisting of questions users interactively answer, and form fields, computed from respective answers. Given the description of a formula, an interactive runtime rendering is generated and automatically updated (for example, new questions are shown if certain fields are selected or within a certain range of answers etc). For a complete description of the scenario consult `./questionnaires/documentation/language-workbench-challenge-2013.pdf`.

The questionnaires of the _RACR_ solution look as follows:

![correct-questionnaire-scenario](questionnaires/documentation/figures/correct-questionnaire-scenario.png)

**Figure 1:** Correct example questionnaire and its rendering.

![erroneous-questionnaire-scenario](questionnaires/documentation/figures/erroneous-questionnaire-scenario.png)

**Figure 2:** Incorrect example questionnaire and its rendering.

Questionnaires, as defined in the _Language Workbench Challenge_, consist of arbitrary many questions. Each question is typed and can be computed, in which case it does not ask users for a value but instead evaluates a given expression and shows the result. Questions can also be part of a group, which means they are only shown if their group condition is _true_. Groups can be nested. Nesting has no further meaning besides combining group conditions. The value of a question, whether computed or user-given, is only visible for succeeding expressions (declare before use). The same question is at most shown once. If it can be shown several times, only its first occurrence is active, i.e., shown to users and its value used in expressions. Questionnaires are statically typed and only well-formed if type correct. The value of unanswered questions is undefined. Computations on undefined yield undefined themselves. If a group condition is undefined, the group is not shown (i.e., the condition is treated to be _false_).

The _RACR_ solution is unique in several ways:

  * The abstract syntax graphs of questionnaires model both, the given and computed information _and_ their graphical representation.
  * The widgets of the GUI are computed by attributes. The actual rendering, i.e., showing and shadowing of questions and updating of computed results, is realised by attributes and rewrites respectively. In doing so, the rendering automatically becomes incremental, i.e., when drawing the GUI, the application will only re-render the GUI elements that changed since their last rendering (for example, if new answers change the value of computed questions or enable or disable groups).
  * Attributes clearly encapsulate all language concerns; a convenient model-view-controller solution is achieved without code mixing, doubling or unnecessary interdependencies. Thereby, the controller is automatically realised by _RACR's_ incremental evaluation.
  * Questionnaires are serialized and deserialized as symbolic-expressions, i.e., executable _Scheme_ programs. If executed, these programs construct the abstract syntax tree representing the respective questionnaire and its current answer state.

The solution uses [_Racket_](http://racket-lang.org) for its GUI implementation for which reason it only works with _Racket_.

**Objective:** Application of RAG-controlled rewriting for the development of interactive runtime models, using attributes for incremental model reasoning and rewrites for model updates.

 * Abstract syntax tree encodes a model of the real world, i.e., its state (questionnaires model questions and their current answers, constraints and GUI representations)
 * Model updates via rewriting (questionnaires are updated according to interactive user answers)
 * Incremental model reasoning and reaction on model updates via attributes (GUI of questionnaires is incrementally rendered using attributes)

## Composed Petri Nets

**Difficult level:** RAG-controlled rewriting experienced, _RACR_ experienced
**New features:** Language extension using specification inheritance and refinement
**Size:** Medium
**Scheme library:** Yes

Extension of the atomic Petri nets language with place fusion semantics, enabling the composition of nets by sharing common places. Composition is based on places marked as special in- and output-ports as presented by

> Wolfgang Reisig. "Simple Composition of Nets".
> In: _Applications and Theory of Petri Nets: 30th International Conference_.
> Volume 5605. Lecture Notes in Computer Science. Springer, June 2009, pages 23-42

A highlight of the example is the automatically optimised enabled analysis thanks to the incremental evaluation semantics of _RACR_. Compositions, executions and decompositions of nets can be arbitrarily intertwined whereas name, well-formedness, composition and enabled analyses are incrementally evaluated.

**Objective:** Validation of the benefits of _RACR's_ incremental evaluation semantics for a non-trivial, interactive scenario with mutual-dependent analyses and transformations (execution semantics of hierarchical Petri nets).

 * Extensive incremental evaluation where rewrites are deduced using complex analyses (composition analysis of Petri nets) and non-obviously influence analyses (enabled analysis must consider tokens added or deleted in non-local, fused places)
 * Systematic language extensions by refining existing analyses and introducing new ones (reuse of atomic Petri net implementation)

## _TTC 2015_: _fUML Activity Diagrams_

**Difficult level:** RAG-controlled rewriting experienced, _RACR_ experienced
**New application:** RAG-controlled rewriting for metamodel semantics and model transformation
**Size:** Medium
**Scheme library:** Yes
**Peer-reviewed:** Published at the [_8th Transformation Tool Contest_](http://www.transformation-tool-contest.eu/2015/)

Implementation of an _[fUML](http://www.omg.org/spec/FUML/) Activity Diagram_ interpreter, the competition scenario of [_The TTC 2015 Model Execution Case_](http://www.transformation-tool-contest.eu/2015/solutions_execution.html) of the [_8th Transformation Tool Contest_](http://www.transformation-tool-contest.eu/2015/). For a detailed description of the contest scenario consult the [original task](ttc-2015-fuml-activity-diagrams/documentation/the-ttc-2015-model-execution-case.pdf).

The implemented activity diagram interpreter reuses the atomic Petri nets language. Attribute-based analyses map _fUML Activity Diagrams_ to Petri nets; their execution coincides with the respective diagram's execution. Since _fUML_ is a subset of the _Unified Modelling Language_, the example demonstrates the application of RAG-controlled rewriting for the specification of metamodel semantics and relates it to well-known metamodelling concepts. The task compromises the development of an executable domain model for _fUML Activity Diagrams_, including their domain concepts with intrinsic and (deduced) extrinsic properties, containment and (deduced) non-containment relations, well-formedness semantics and transformational execution semantics.

The interpreter won the [_Overall Quality Award_](ttc-2015-fuml-activity-diagrams/documentation/solution-award.pdf) of _The TTC 2015 Model Execution Case_ and is accompanied by a peer-reviewed paper explaining its implementation in detail:

> Christoff Bürger.
> "fUML Activity Diagrams with RAG-controlled Rewriting: A RACR Solution of The TTC 2015 Model Execution Case".
> In: _TTC 2015: Proceedings of the 8th Transformation Tool Contest_.
> Volume 1524. CEUR Workshop Proceedings. CEUR-WS.org, July 2015, pp. 27–36.
> [Original version at CEUR-WS.org](http://ceur-ws.org/Vol-1524/paper10.pdf), [up-to-date version](ttc-2015-fuml-activity-diagrams/documentation/solution-description.pdf)

**Objective:** Application of RAG-controlled rewriting for metamodelling, using attributes and rewrites for non-transformational and transformational semantics respectively.

 * Abstract syntax tree scheme to model domain concepts, their intrinsic properties and structural containment relations
 * (Reference) attributes to model extrinsic, deduced properties and relations
 * Attribute-based, deduced rewrites to model transformations

**Documentation:** A [poster](ttc-2015-fuml-activity-diagrams/documentation/solution-poster-a0.pdf) and [presentation](ttc-2015-fuml-activity-diagrams/documentation/solution-presentation.pdf) summarise the _RACR_-based implementation of the _fUML Activity Diagram_ interpreter. A [peer-reviewed paper](ttc-2015-fuml-activity-diagrams/documentation/solution-description.pdf) explains and evaluates it in detail.

## _SiPLE_ (Simple imperative Programming Language Example)

**Difficult level:** Reference attribute grammar experienced, Rewriting novice, _RACR_ experienced
**Size:** Medium
**Scheme library:** Yes

_SiPLE_ is a simple imperative programming language. Its language concepts are:

  * Integer, real and Boolean arithmetics
  * Pointers (including pointers to procedures and pointers)
  * A block-structured name space and nested procedures
  * Lexically-scoped first-class functions (i.e., lexical closures)
  * `While` and `If` control-flow statements
  * Automatic integer to real type coercions (e.g., when assigning an integer value to a variable of type real or when adding an integer to a real value)
  * A save interpreter, that calmly terminates in the presence of errors

_SiPLE_ is strongly typed, such that a static type analysis can be performed.
