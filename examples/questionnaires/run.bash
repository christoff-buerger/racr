#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

######################################################################################################### Execute questionnaires:
if [ $# -ge 1 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi
"$script_dir/../../run-program.bash" -s racket -e "$script_dir/run.scm"
