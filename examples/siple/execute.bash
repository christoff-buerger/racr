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
	"$script_dir/execute.bash" -h
	exit $?
fi

selected_systems=()

while getopts xs:e:h opt
do
	case $opt in
		x)
			execute_incorrect=":true:"
			;;
		s)
			selected_systems+=( -s )
			selected_systems+=( "$OPTARG" )
			;;
		e)
			if [ -z ${program+x} ]
			then
				program="$OPTARG"
			else
				echo " !!! ERROR: Several SiPLE programs for execution selected via -e parameter !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -s Scheme system (optional parameter). Permitted values:" >&2
			"$script_dir/../../deploying/deployment-scripts/list-scheme-systems.bash" -k | \
				sed 's/^/             /' >&2
			echo "          By default, GNU Guile is used." >&2
			echo "       -e SiPLE program to interpret (mandatory parameter)." >&2
			echo "       -x Expect interpretation error (optional flag)." >&2
			echo "          Abort with an error if no runtime error is encountered throughout interpretation." >&2
			echo "          By default, a correct SiPLE program is expected." >&2
			echo "       -- Command line arguments for the SiPLE program to interpret (optional parameter)." >&2
			echo "          All following arguments are forwarded." >&2
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

if [ -z ${execute_incorrect+x} ]
then
	execute_incorrect=":false:"
fi

if [[ ! -v selected_systems[@] ]]
then
	selected_systems+=( -s )
	selected_systems+=( "guile" )
fi

if [ -z ${program+x} ]
then
	echo " !!! ERROR: No SiPLE program to interpret given via -e parameter !!!" >&2
	exit 2
fi

########################################################################################################## Execute SiPLE program:
"$script_dir/../../deploying/deployment-scripts/execute.bash" "${selected_systems[@]}" \
	-e "$script_dir/execute.scm" -- "$program" "$execute_incorrect" "$@"
