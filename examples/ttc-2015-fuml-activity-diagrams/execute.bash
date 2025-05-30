#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if (( $# == 0 ))
then
	"$script_dir/execute.bash" -h
	exit $?
fi

selected_systems=()

while getopts cxs:d:i:m:h opt
do
	case $opt in
		c)
			cache_enabled_analysis=":false:"
			;;
		x)
			print_trace=":false:"
			;;
		s)
			selected_systems+=( -s )
			selected_systems+=( "$OPTARG" )
			;;
		d)
			if [[ ! -v "diagram" ]]
			then
				diagram="$OPTARG"
			else
				echo " !!! ERROR: Several activity diagrams for execution selected via -d parameter !!!" >&2
				exit 2
			fi
			;;
		i)
			if [[ ! -v "input" ]]
			then
				input="$OPTARG"
			else
				echo " !!! ERROR: Several diagram inputs for execution selected via -i parameter !!!" >&2
				exit 2
			fi
			;;
		m)
			if [[ ! -v "mode" ]]
			then
				mode="$OPTARG"
			else
				echo " !!! ERROR: Several modes for diagram execution selected via -m parameter !!!" >&2
				exit 2
			fi
			;;
		h|*)
			echo "Usage: -s Scheme system (optional parameter). Permitted values:" >&2
			"$script_dir/../../deploying/deployment-scripts/list-scheme-systems.bash" -i | \
				sed 's/^/             /' >&2
			echo "          By default, Chez Scheme is used." >&2
			echo "       -d Activity diagram (mandatory parameter)." >&2
			echo "       -i Activity diagram input (optional parameter)." >&2
			echo "       -m Execution mode (optional parameter). Permitted values:" >&2
			echo "             1=parsing" >&2
			echo "             2=AD-well-formedness" >&2
			echo "             3=PN-generation" >&2
			echo "             4=PN-well-formedness" >&2
			echo "             5=PN-enabled" >&2
			echo "             6=PN-execution (no enabled passes)" >&2
			echo "             7=PN-execution (use enabled passes)" >&2
			echo "          The default is 6: Petri net execution, one transition each step." >&2
			echo "       -c Deactivate caching of enabled analysis (optional multi-flag)." >&2
			echo "          By default, the enabled analysis of generated Petri nets is cached." >&2
			echo "       -x Deactivate printing the execution trace on stdout (optional multi-flag)." >&2
			echo "          By default, the execution trace is printed." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if (( $# != 0 ))
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [[ ! -v "cache_enabled_analysis" ]]
then
	cache_enabled_analysis=":true:"
fi

if [[ ! -v "print_trace" ]]
then
	print_trace=":true:"
fi

if (( ${#selected_systems[@]} == 0 ))
then
	selected_systems+=( -s )
	selected_systems+=( "chez" )
fi

if [[ ! -v "diagram" ]]
then
	echo " !!! ERROR: No activity diagram to execute given via -d parameter !!!" >&2
	exit 2
fi

if [[ ! -v "input" ]]
then
	input=":false:"
fi

if [[ ! -v "mode" ]]
then
	mode=6
elif (( "$mode" < 1 || "$mode" > 7 ))
then
	echo " !!! ERROR: No valid execution mode selected !!!" >&2
	exit 2
fi

####################################################################################################### Execute activity diagram:
"$script_dir/../../deploying/deployment-scripts/execute.bash" "${selected_systems[@]}" -e "$script_dir/execute.scm" -- \
	"$diagram" "$input" "$mode" "$cache_enabled_analysis" "$print_trace"
