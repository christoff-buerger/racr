#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# Function always called when the script terminates:
my_exit(){
	rm script.scm
	exit 0
}
trap 'my_exit' 1 2 3 9 15

echo "#!r6rs" > script.scm
echo "(import (questionnaires language) (questionnaires user-interface))" >> script.scm
echo "(load-questionnaire)" >> script.scm

racket -S ../../racr/racket-bin -S racket-bin script.scm #plt-r6rs ++path ../../racr/racket-bin ++path racket-bin script.scm
my_exit
