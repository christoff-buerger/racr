#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/check-tables.bash" -h
	exit $?
fi

while getopts c:h opt
do
	case $opt in
		c)
			if [ "${profiling_configuration+x}" = "" ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -- List of measurements tables to check (mandatory parameter)." >&2
			echo "          Must be non-empty." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

measurements_tables=( "$@" )
shift ${#measurements_tables[@]}
for t in "${measurements_tables[@]}"
do
	if [ ! -f "$t" ]
	then
		echo " !!! ERROR: Non-existing measurements table [$t] specified via '--' argument list !!!" >&2
		exit 2
	fi
done
if (( ${#measurements_tables[@]} == 0 ))
then
	echo " !!! ERROR: No measurements table specified via '--' argument list !!!" >&2
	exit 2
fi

###################################################################################################### Check measurements tables:
for t in "${measurements_tables[@]}"
do
	"$script_dir/record.bash" -c "$profiling_configuration" -t "$t" -x
done

exit 0
