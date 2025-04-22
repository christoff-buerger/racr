#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

######################################################################################################### Execute questionnaires:
if (( $# != 0 ))
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

"$script_dir/../../deploying/deployment-scripts/execute.bash" -s racket -e "$script_dir/execute.scm"
