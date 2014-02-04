RACR-C
======


### 1. HOW TO BUILD ###

#### 1.1 RACKET ####

Racr-c uses racket's virtual machine to load and execute r6rs scheme code.
It also utilizes racket's conservative garbage collector library `libmzgc.a` which, unfortunately,
is not provided through racket's binary package.

Thus, in order to build racr-c, racket is required to be built afresh from its sources.
It is most likely easiest to simply execute the following commands:

	$ git clone https://github.com/plt/racket/
	$ cd racket
	$ make # this may take some time

The Makefile assumes to find the repository in `../../racket/`.
You may wish to adjust the Makefile by setting the variable `RACKET_PATH` according to your setup.








	TODO: bytecode
