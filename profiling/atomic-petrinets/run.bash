#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

profiling_configuration="$script_dir/profiling-configuration"
configuration_to_parse="$script_dir/racr-library-configuration"
. "$script_dir/../profiling-scripts/configure.bash" # Sourced script sets configuration!
. "$script_dir/../../parse-configuration.bash" # Sourced script sets configuration!

if [ $# -ne $number_of_parameters ]
then
	echo "Usage: The following $number_of_parameters command line arguments are expected:" >&2
	for (( i = 0, j = 1; i < number_of_parameters; i++, j++))
	do
		echo "             $j: ${parameter_names[$i]} (${parameter_descriptions[$i]})" >&2
	done
	echo "       The Scheme system for execution must be one of:" >&2
	for (( i = 0; i < ${#supported_systems[@]}; i++))
	do
		echo "             ${supported_systems[$i]}" >&2
	done
	echo "       All other arguments must be natural numbers." >&2
	echo "       For some combinations no respective profiling Petri net can be generated." >&2
	echo "       The measurement will abort with an error for these cases." >&2
	exit 2
fi

selected_system="$1"
shift

exec 3>&1 4>&2
execution_time=$(
	TIMEFORMAT=%R
	{
		time "$script_dir/../../run-program.bash" -s "$selected_system" -e "$script_dir/run.scm" -- "$@" 1>&3 2>&4
	} 2>&1
)
exec 3>&- 4>&-
echo $execution_time
