_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Requirements and Installation

_RACR_ and its examples are self-contained _R6RS Scheme_ programs; they work with any _R6RS_ conformant _Scheme_ system. No additional _SRFI_ or _Scheme_ libraries are required. _RACR_ has been tested with the following _Scheme_ systems:
  * [Racket 6.1.1](http://www.racket-lang.org/) ([GitHub repository](https://github.com/plt/racket))
  * [Larceny 0.98](http://www.larcenists.org) ([GitHub repository](https://github.com/larcenists/larceny))
  * [GNU Guile 2.0.11](http://www.gnu.org/software/guile/)
  * [IronScheme](http://ironscheme.codeplex.com) ([GitHub repository](https://github.com/leppie/IronScheme))
  * [Chez Scheme 8.4](http://www.scheme.com/)

_RACR_ and some of its examples are provided as _Scheme_ libraries. The instantiation of libraries differs between different _Scheme_ systems. Consult the documentation of your _Scheme_ system in case of any instantiation issues. Besides proper instantiation as _Scheme_ library, no further configurations are required to use _RACR_.

## Installation _Bash_ Shell Script

The `install.bash` _Bash_ shell script distributed with _RACR's_ source code can be used to compile and ease the loading of _RACR_ for the above mentioned _Scheme_ systems. It detects if the system's executable to load/compile _R6RS_ libraries is in your path and performs all necessary compilation and configuration actions. If throughout the process any artefacts are generated like binaries, they are stored in `name-of-scheme-system-bin` directories which are contained in the directory with the respective library source files. Thus, all generated artefacts are local to _RACR's_ source code distribution.

Note, that the script installs all libraries distributed with _RACR_, including its [examples](../examples/examples-overview.md). It handles libraries only; the ordinary top-level programs distributed with _RACR_ are not considered.

To get an idea how to use _RACR's_ libraries in your _Scheme_ system, consult the `run-tests.bash` script. The script validates the installation by running a set of test cases.
