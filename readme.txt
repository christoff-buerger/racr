                                    RACR
               Reference-Attribute-Grammar-Controlled Rewriting
                              Christoff BŸrger
                         Christoff.Buerger@gmx.net
===============================================================================

RACR is a Scheme attribute grammar (AG) library providing incremental attribute
evaluation in the presence of arbitrary abstract syntax tree (AST) rewrites.
RACR library functions can be used to specify AST schemes and their attribution
and construct respective ASTs, access their attributes and perform rewrites.

Since RACR is an ordinary Scheme library, RACR AGs can arbitrarily interact
with Scheme programs and vice versa. Attribute equations are just user
specified Scheme functions and the RACR evaluator automatically maintains
attribute dependencies and caching. Arbitrary control-flows within attribute
equations can be handled, including function calls, macro expansions and
continuations (i.e., RACR is control-flow safe).

Beside synthesized and inherited attributes, RACR supports reference,
parameterized and circular attributes, attribute broadcasting and AST and
attribute inheritance. Attribute evaluation is demand driven and incremental.
Thus, attributes are only evaluated if required, either because their value is
requested for the first time or because it might have changed by an AST
rewrite. Thereby, only the actual attribute dependencies of the concrete AST
are considered not a static dependency approximation. I.e., RACR performs a
dynamic attribute dependency analysis incorporating control-flow within
equations.

                             Short Feature List
*******************************************************************************

AST Specifications:
 - Typed non-terminals
 - EBNF (Kleene closure / AST lists)
 - Non-terminal (i.e., node type) inheritance
 - AST annotations (weaving of arbitrary entities into ASTs)

AG Specifications:
 - Synthesized and inherited attributes
 - Circular attributes
 - Parameterized attributes
 - Attribute broadcasting
 - Attribute equation inheritance w.r.t. AST inheritance hierarchy
 - Automatic attribute caching
 - Incremental attribute evaluation
 
Rewrite API permitting:
 - AST node and subtree replacements
 - Insertion, deletion and addition of list elements
 - Terminal replacement
 - Automatic attribute dependency and cache maintenance in the presence
   of rewrites

                       Installation and Prerequisites
*******************************************************************************

The RACR library and its examples are self-contained R6RS Scheme programs and
should compile with any R6RS conformant Scheme system. No additional SRFI or
Scheme libraries are required. RACR has been tested with the following Scheme
distributions:
 - Racket 5.1.3
 - Chez Scheme 8.3
 - Mosh 0.2.5

The RACR system and some of its examples are provided as Scheme libraries. The
installation and configuration of libraries differs between different Scheme
distributions. Consult the documentation of your Scheme system in case of any
installation issues.

                                   Examples
*******************************************************************************

Newcomers should start by investigating the binary-numbers example within the
examples folder and compare its AST and attribute specifications with the one
of Knuth's classical attribute grammar paper:

                    "Semantics of Context-Free Languages",
                               Donald E. Knuth,
          Theory of Computing Systems, volume 2, number 2, pages 127-145,
                               Springer, 1968

The state machines example illustrates the advantages and usage of reference
and circular attributes. Within the example a circular attribute is used to
compute for each state, which states are reachable from it (states transitive
closure). Remember, that RACR does not only automatically handle fix-point
computations, but additionally attribute caching, including circular and
parameterized attributes.

If you feel familiar with attribute grammars just investigate the SiPLE
example, a classical simple imperative programming language example in the
spirit of Oberon. It shows some of the more advanced features like
parameterized and circular attributes, broadcasting, inheritance and AST
weaving. Its type coercions are also a first example for simple rewrites. The
interpreter is an example for RACR's AST annotation capabilities; in case of
the interpreter execution functions with side effects, which reuse static
semantics for interpretation purposes, are woven into ASTs.

                                   License
*******************************************************************************

This program and the accompanying materials are made available under the terms
of the MIT license (X11 license) which accompanies this distribution.

                                    TODO
*******************************************************************************

In the long run, RACR will be extended to a real rewrite system. Additionally
to the current rewrite API users will be able to specify arbitrary many
rewrite rules similarly to the existing AST and AG specifications. The desired
rewrite specifications will not only permit the matching of AST structures, but
also of reference attribute edges and rewrite constraints specified in terms of
attributes. AST, AG and rewrite rule specifications will be compiled together
yielding an incremental AG system with automatic rewriting. Users accessing
ASTs and their attributes will only see the final ASTs resulting after the
application of all applicable rewrites, i.e., AST normalforms in terms of
rewriting. To archive confluence, attribute dependency graphs will be extended
to rewrite depenency graphs that control the order of rewrite applications.
In case of circular rewrite dependencies users are responsible to provide
disambiguation functions (cost functions) selecting a certain rewrite to
prefer. A default heuristic for disambiguation prefering outermost rewrites
(similar to normal-order evaluation in the Lambda calculus) will be provided.

The abstract for RACR will be:

RACR is a Scheme compiler generator library for the specification of abstract
syntax trees (AST), Attribute-Grammar-based static semantics (AG) and AST
rewrite rules (RR). Thereby, rewrite rules can reuse static semantics (i.e.,
attributes), either to extend left-hand sides to graphs via reference
attributes or within rewrite conditions. Given a set of AST, AG and RR
specifications, the RACR system can be used to compile these specifications
and provide a set of AST constructor functions. Any AST constructed using
these functions can be queried for its attribute values, i.e., its semantics.
Thereby, the RACR system applies all applicable rewrites considering rewrite
dependencies and disambiguation. In case of non-cyclic rewrite conflicts,
disambiguation can be performed automatically. In any other case, cost
functions disambiguating mutual dependent rewrites must be specified. To ease
the specification of deterministic rewrite systems, a default cost function can
be used.
