#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
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
			if [ -z ${profiling_configuration+x} ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -- List of source tables to check (mandatory parameter)." >&2
			echo "          Must be non-empty." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

for a in "$@"
do
	if [ ! -f "$a" ]
	then
		echo " !!! ERROR: Non-existing source table [$a] specified via '--' argument list !!!" >&2
		exit 2
	fi
	source_tables+=( "$a" )
	shift
done
if [ -z ${source_tables+x} ]
then
	echo " !!! ERROR: No source table specified via '--' argument list !!!" >&2
	exit 2
fi

####################################################################################################### Check measurement tables:
for t in "${source_tables[@]}"
do
	"$script_dir/record.bash" -c "$profiling_configuration" -t "$t" -x
done

exit 0
