#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

############################################################################### Configure temporary resources & execution script:
old_pwd=`pwd`

my_exit(){
	cd "$old_pwd"
	rm script.scm
	exit 0
}
trap 'my_exit' 1 2 3 9 15

echo "#!r6rs" > script.scm
echo "(import (questionnaires language))" >> script.scm
echo "(load-questionnaire)" >> script.scm

######################################################################################################### Execute questionnaires:
../../run-program.bash -s racket -e script.scm
my_exit
