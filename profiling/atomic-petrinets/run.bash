#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

selected_system=$1
shift

exec 3>&1 4>&2
execution_time=$(
	TIMEFORMAT=%R
	{
		time "$script_dir/../../run-program.bash" -s $selected_system -e "$script_dir/run.scm" -- "$@" 1>&3 2>&4
	} 2>&1
)
exec 3>&- 4>&-
echo $execution_time
