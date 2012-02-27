                                    RACR
               Reference-Attribute-Grammar-Controlled Rewriting
                              Christoff BŸrger
                         Christoff.Buerger@gmx.net
===============================================================================

RACR is a scheme compiler generator library for the specification of abstract
syntax trees (AST), Attribute-Grammar-based static semantics (AG) and AST
transformations/rewrite rules (RR). Thereby, rewrite rules can reuse static
semantics (i.e., attributes), either to extend left-hand sides to graphs via
reference attributes or within rewrite conditions. Given an AST, AG and RR
specification, the RACR system can be used to compile these specifications
and provide a set of AST constructor functions. Any AST constructed using these
functions can be queried for its attribute values, i.e., its semantics.
Thereby, the RACR system applies all applicable rewrites considering rewrite
dependencies and disambiguation. In case of non-cyclic rewrite conflicts,
disambiguation can be performed automatically. In any other case, cost
functions disambiguating mutual dependent rewrites must be specified. To ease
the specification of deterministic rewrite systems, a default cost function can
be used.

                             Short Feature List
*******************************************************************************

AST Specifications:
 - Typed terminals & non-terminals
 - EBNF (Kleene closure)
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
 
RR Specifications: TODO

                       Installation and Prerequisites
*******************************************************************************

The RACR library and its examples use only R6RS Scheme functionallities and
should compile with any R6RS conformant Scheme system. The following
distributions have been tested:
 - Racket 5.1.3
 - Chez Scheme 8.3
 - Mosh 0.2.5

The RACR system and all its examples are provided as Scheme libraries. However,
the installation and configuration of libraries differs between different
Scheme distributions; please consult your system's manual.

For Racket and Chez Scheme installation scrips are provided. BEFORE using these
scripts, please configure their absolut paths to your Racket/Chez Scheme
distribution.

                                   Examples
*******************************************************************************

Newcomers should start by investigating the binary-numbers example within the
examples folder and compare its AST and attribute specifications with the one
of Knuth's classical attribute grammar paper:

                    "Semantics of Context-Free Languages",
                               Donald E. Knuth,
          Theory of Computing Systems, volume 2, number 2, pages 127-145,
                               Springer, 1968

The state machines example illustrates the advantages and usage of circular
attributes. Within the example a circular attribute is used to compute for
each state, which states are reachable from it (states' transitive closure).
Remember, that RACR does not only automatically handle fix-point
computations, but additionally attribute caching, including circular and
parameterized ones.

If you feel familiar with attribute grammars just investigate the SiPLE
example, a classical simple imperative programming language example in the
spirit of Oberon. It shows some of the more advanced features like
parameterized and circular attributes, broadcasting, inheritance and AST
weaving. Its type coercions are also a first example for simple rewrites. For
advanced rewrite specifications consider the SiPLE optimizer. The interpreter
is an example for RACR's AST annotation capabilities; in case of the
interpreter interpret functions with side effects, which reuse SiPLE's static
semantics for interpretation purposes, are woven into ASTs.

                                   License
*******************************************************************************

This program and the accompanying materials are made available under the terms
of the MIT license (X11 license) which accompanies this distribution.

                                    TODO
*******************************************************************************

 - Everything that has to do with rewriting.
