#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

############################################################################### Configure temporary resources & execution script:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

my_exit(){
	rm "$script_dir/start-script.scm"
	exit 0
}
trap 'my_exit' 1 2 3 9 15

echo "#!r6rs" > "$script_dir/start-script.scm"
echo "(import (questionnaires language))" >> "$script_dir/start-script.scm"
echo "(load-questionnaire)" >> "$script_dir/start-script.scm"

######################################################################################################### Execute questionnaires:
"$script_dir/../../run-program.bash" -s racket -e "$script_dir/start-script.scm"
my_exit
