_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Requirements

_RACR_ and its examples are self-contained _R6RS Scheme_ programs; they work with any _R6RS_ conformant _Scheme_ system. No additional _SRFI_ or _Scheme_ libraries are required. _RACR_ has been tested with the following _Scheme_ systems:
  * [Racket 6.3](http://www.racket-lang.org/) ([GitHub repository](https://github.com/plt/racket))
  * [Larceny 0.98](http://www.larcenists.org) ([GitHub repository](https://github.com/larcenists/larceny))
  * [GNU Guile 2.0.11](http://www.gnu.org/software/guile/)
  * [IronScheme 115672, 20th November 2015](http://ironscheme.codeplex.com) ([GitHub repository](https://github.com/leppie/IronScheme))
  * [Chez Scheme 8.4](http://www.scheme.com/)

_RACR_ and some of its examples are provided as _Scheme_ libraries. The instantiation of libraries differs between different _Scheme_ systems. Consult the documentation of your _Scheme_ system in case of any instantiation issues. Besides proper instantiation as _Scheme_ library, no further configurations are required to use _RACR_.

## Installation, Test and Execution _Bash_ Shell Scripts

The `install-libraries.bash` _Bash_ shell script distributed with _RACR's_ source code can be used to compile and ease the loading of _RACR_ for the above mentioned _Scheme_ systems. It performs all necessary compilation and configuration actions. It does not change anything outside of _RACR's_ source code distribution. If called without any arguments, all libraries distributed with _RACR_, including its [examples](../../examples/examples-overview.md) are installed for the _Scheme_ systems found. The script can also be used to install just _RACR_ or one of its examples and installation can be restricted to a certain _Scheme_ system. For an explanation how to do so, just call `./install-libraries.bash -h`.

To validate the installation, the `run-tests.bash` script can be used. Similarly to the installation-script, the tests can be executed using only a certain _Scheme_ system (for details call `run-tests.bash -h`). If no system is specified, all available systems are tested.

The `run-program.bash` scrip can be used to execute _RACR_-based programs with any of the officially supported _Scheme_ systems. It provides flags to select the _Scheme_ program to execute, the _Scheme_ system to use for execution and the library to load for execution (for details call `run-program.bash -h`). Surplus arguments are passed to the executed program. The script uses _RACR_-library configuration files to set up the execution environment.

Some of the [provided examples](../../examples/examples-overview.md) require a proper setup by the caller like input file arguments. These examples ship with a `run.bash` script that parses command line arguments, tests their validity, provides proper error messages, properly configures the environment and, if everything is fine, finally executes the example. Such execution scripts are found in the directory containing the source code of the respective example (for example `examples/questionnaires/run.bash`).

## _.NET_ Installation and Tests

_RACR_ provides a _.NET_ integration called [RACR-NET](../../racr-net/documentation/title.md), which is based on _C#_ and [IronScheme](http://ironscheme.codeplex.com). The artefacts related to _RACR-NET_ are in `*-net` directories. _C#_ example programs using _RACR-NET_ are in the `examples-net` directory, tests are in the `tests-net` directory, the implementation of _RACR-NET_ itself is in the `racr-net` directory etc. As known from the _Microsoft Build Tools_ and _C#_, `csproj` files are used for building _RACR-NET_, its examples and tests.

_RACR-NET_ is based on _RACR_. It requires _RACR's_ _Scheme_ libraries as _IronScheme_ _.NET_ assemblies (dynamic linked libraries compiled by _IronScheme_ and depending on it). The `install-libraries.bash` script generates the respective `dll` assemblies in the `racr-net/ironscheme-bin` directory (the `racr.`_*_`.dll` files), if used to install _RACR_ for _IronScheme_. A local _IronScheme_ distribution must be available however; its root directory must be part of the `PATH` environment variable. Building _RACR-NET_ via `racr-net/Racr.csproj` yields the _Racr.dll_ _IronScheme_ _.NET_ assembly in `racr-net/ironscheme-bin`. The _IronScheme_ assembly, _RACR_ assemblies and _RACR-NET_ assembly are the minimal set of assemblies required to use _RACR_ in _C#_ via the [_RACR-NET_ API](../../racr-net/documentation/title.md).
