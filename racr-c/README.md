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

The Makefile looks for the repository in `../../racket/`.
You may wish to adjust the Makefile by setting the variable `RACKET_PATH` according to your setup.
Throughout the remaining document, it is assumed that the environment variable `RACKET_PATH` be valid.



#### 1.2 RACR-C ####

A racr-c application must be linked to `libracket.a` and `libmzgc.a`, both of which residing in
`$RACKET_PATH/racket/src/build/racket/`. These libraries, in turn, require `-lm -lpthread -ldl -lffi`.

...



#### 1.3 BYTE-CODE ####

Any racr-c application needs a byte-code file containing racr's encoded scheme code,
as well as any library's which you may wish to access from within racr-c.
With the aid of `$RACKET_PATH/racket/bin/raco` and `gcc`, it is generated through executing the script `bin/make_bc`.
However, all desired libraries must first be properly installed.


1. Install scheme libraries into racket:

	$ $RACKET_PATH/racket/bin/plt-r6rs --install ../racr/racr.scm
	$ $RACKET_PATH/racket/bin/plt-r6rs --install ../racr/racr-test-api.scm

You may likewise also install any other libraries:

	$ $RACKET_PATH/racket/bin/plt-r6rs --install my-scm-lib.scm


2. Generate the byte-code file:

	$ bin/make_bc bc

If you wish to include libraries of you own, do this instead:

	$ bin/make_bc bc my-scm-lib




