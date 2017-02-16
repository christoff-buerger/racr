# Synopsis: _RACR-NET_ overhead benchmarks

This directory contains examples tailored to benchmark the overhead of using _RACR-NET_ compared to _RACR_. Overhead incurs to maintain the mapping of the in-memory representations of abstract syntax tree nodes between _C#_ and _Scheme_ and for automatic marshalling of common types. The used examples have the following characteristics:
 * Attribute equations are negligible; the computations for a single attribute value are trivial and insignificant compared to the infrastructure checking for cache hits, runtime errors and consistency in the presence of rewrites.
 * Rewrites are excessively applied, resulting in an unusual high amount of attribute re-evaluations.

In the end, the examples spend most computation time for _RACR_ internals, including the integrity of the _RACR-NET_ interface. This enables reliable overhead estimations by comparing plain _RACR_ implementations with their respective _RACR-NET_ solutions. Of course, this application scenario is **NOT** representative to evaluate _RACR_ as such, neither its computational nor its development performance.
