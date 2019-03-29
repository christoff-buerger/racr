_[>> Title <<](title.md) [>> Synopsis <<](synopsis.md) [>> Contents <<](contents.md) [>> API Index <<](api-index.md)_
___

# Requirements

_RACR_ and its examples are self-contained _R6RS Scheme_ programs; they work with any _R6RS_ conformant _Scheme_ system. No additional _SRFI_ or _Scheme_ libraries are required. _RACR_ has been tested with the following _Scheme_ systems:
  * [_GNU Guile_ 2.2.4](http://www.gnu.org/software/guile/)
  * [_Larceny_ 1.3](http://www.larcenists.org) ([_GitHub_ repository](https://github.com/larcenists/larceny))
  * [_Chez Scheme_ 9.5.2](http://www.scheme.com/) ([_GitHub_ repository](https://github.com/cisco/ChezScheme))
  * [_Racket_ 7.2](http://www.racket-lang.org/) ([_GitHub_ repository](https://github.com/plt/racket))
  * [_Sagittarius Scheme_ 0.9.4](https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home) ([_GitHub_ repository](https://github.com/ktakashi/sagittarius-scheme))
  * [_IronScheme_ 1.0.102-de0d45c, 10th January 2017](https://archive.codeplex.com/?p=ironscheme) ([_GitHub_ repository](https://github.com/leppie/IronScheme))

_RACR_ and some of its examples are provided as _Scheme_ libraries. The instantiation of libraries differs between different _Scheme_ systems. Consult the documentation of your _Scheme_ system in case of any instantiation issues. Besides proper instantiation as _Scheme_ library, no further configurations are required to use _RACR_.

# Deployment _Bash_ Shell Scripts

To ease the deployment of _RACR_ and libraries developed using it, _Bash_ shell scripts are provided in the `deploying/deployment-scripts` directory.

**Installation** The `install.bash` script can be used to compile and install _RACR_ and _Scheme_ libraries using _RACR_ for different _Scheme_ systems. The script performs all necessary compilation and configuration actions. It does not change anything outside of _RACR's_ source code distribution. If called without any arguments, all libraries distributed with _RACR_, including its [examples](../../examples/examples-overview.md) are installed for all _Scheme_ systems found. The script can also be used to install only selected libraries and installation can be restricted to a certain _Scheme_ system. For an explanation how to do so, just call `./install.bash -h`.

**Tests** To validate the installation, the `tests/execute.bash` script can be used. Similarly to the installation script, the tests can be executed using only a certain _Scheme_ system (for details call `tests/execute.bash -h`). If no system is specified, all available systems are tested.

**Execution** The `execute.bash` scrip can be used to execute _RACR_-based programs with any of the officially supported _Scheme_ systems. It provides flags to select the _Scheme_ program to execute, the _Scheme_ system to use for execution and the library to load for execution (for details call `execute.bash -h`). Surplus arguments separated by `--` are passed to the executed program. The script uses _RACR_-library configuration files to set up the execution environment.

Some of the [provided examples](../../examples/examples-overview.md) require a proper setup by the caller like input file arguments. These examples ship with a `execute.bash` script that parses command line arguments, tests their validity, provides proper error messages, properly configures the environment and, if everything is fine, finally executes the example. Such execution scripts are found in the directory containing the source code of the respective example (for example `examples/questionnaires/execute.bash`).

**Installation overview** For an overview of installed and officially supported _Scheme_ systems and _RACR_-libraries the `list-scheme-systems.bash` and `list-libraries.bash` scripts can be used.

# _.NET_ Installation and Usage

_RACR_ provides a _.NET_ integration with a dedicated _C#_ API called _RACR-NET_. For an detailed overview of _RACR-NET_ its [reference manual](../../racr-net/documentation/title.md) can be consulted. The subject in the following just is its installation.

_RACR-NET_ is tested to run with [_Mono 5.16.1.0_](https://www.mono-project.com) and is based on [_IronScheme_](https://archive.codeplex.com/?p=ironscheme). The artefacts related to _RACR-NET_ are in `*-net` directories. _C#_ example programs using _RACR-NET_ are in the `examples-net` directory, tests are in the `tests-net` directory, the implementation of _RACR-NET_ itself is in the `racr-net` directory etc. The _Microsoft Build Tools_ and `csproj` files are used for building _RACR-NET_, its examples and tests.

_RACR-NET_ provides for each of _RACR's_ functionalities an respective abstraction in _C#_. Internally, _RACR-NET_ calls _RACR_ in turn; to that end it requires _RACR's_ _Scheme_ libraries as _IronScheme_ _.NET_ assemblies (dynamic linked libraries compiled by _IronScheme_ and depending on it). The `install.bash` script can be used to generate the respective `dll` assemblies. A local _IronScheme_ distribution must be available however; its root directory must be part of the `PATH` environment variable. Afterterwards, _RACR-NET_ itself can be build via the `racr-net/Racr.csproj` build script. The resulting `Racr.dll` in `racr-net/binaries` together with the _IronScheme_ and _RACR_ assemblies are all assemblies required to use _RACR-NET_.
