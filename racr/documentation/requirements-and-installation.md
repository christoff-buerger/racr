_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Requirements and Installation

_RACR_ and its examples are self-contained _R6RS Scheme_ programs; they work with any _R6RS_ conformant _Scheme_ system. No additional _SRFI_ or _Scheme_ libraries are required. _RACR_ has been tested with the following _Scheme_ systems:
  * [Racket 6.3](http://www.racket-lang.org/) ([GitHub repository](https://github.com/plt/racket))
  * [Larceny 0.98](http://www.larcenists.org) ([GitHub repository](https://github.com/larcenists/larceny))
  * [GNU Guile 2.0.11](http://www.gnu.org/software/guile/)
  * [IronScheme](http://ironscheme.codeplex.com) ([GitHub repository](https://github.com/leppie/IronScheme))
  * [Chez Scheme 8.4](http://www.scheme.com/)

_RACR_ and some of its examples are provided as _Scheme_ libraries. The instantiation of libraries differs between different _Scheme_ systems. Consult the documentation of your _Scheme_ system in case of any instantiation issues. Besides proper instantiation as _Scheme_ library, no further configurations are required to use _RACR_.

## Installation and Test _Bash_ Shell Scripts

The `install.bash` _Bash_ shell script distributed with _RACR's_ source code can be used to compile and ease the loading of _RACR_ for the above mentioned _Scheme_ systems. It performs all necessary compilation and configuration actions. It does not change anything outside of _RACR's_ source code distribution. If called without any arguments, all libraries distributed with _RACR_, including its [examples](../../examples/examples-overview.md) are installed for the _Scheme_ systems found. The script can also be used to install just _RACR_ or one of its examples and installation can be restricted to a certain _Scheme_ system. For an explanation how to do so, just call `./install.bash -h`.

To validate the installation, the `run-tests.bash` script can be used. Similarly to the installation-script, the tests can be executed using only a certain _Scheme_ system (for details call `run-tests.bash -h`). If no system is specified, all available systems are tested.

## Execution _Bash_ Shell Script

## _.NET_ Installation

_RACR_ provides a _.NET_ integration called [RACR-NET](../../racr-net/documentation/title.md), which is based on _C#_ and [IronScheme](http://ironscheme.codeplex.com). The artefacts related to _RACR-NET_ are in `*-net` directories. _C#_ example programs using _RACR-NET_ are in the `examples-net` directory, tests are in the `tests-net` directory, _RACR-NET_ itself is in the `racr-net` directory etc.

As known from the _Microsoft Build Tools_ and _C#_, `csproj` files are used for building _C#_ code, including _RACR-NET's_ examples and tests. The required libraries are provided as dynamic linked libraries in the `racr-net` directory (`Racr.dll` and `IronScheme.dll`).

**Beware:** _Only RACR-NET developers, and not users, should build the `Racr.dll` using the scripts provided in its directory. A local IronScheme distribution must be available; the distributed `IronScheme.dll` is not sufficient._
