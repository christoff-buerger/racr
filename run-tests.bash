#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
while getopts s:h opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems+=( "$OPTARG" );;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter). Permitted values:" >&2
			echo "`"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /'`" >&2
			echo "          If no system is selected, all installed and officially supported systems are tested." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$@] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${selected_systems+x} ]
then
	selected_systems=`"$script_dir/list-scheme-systems.bash" -i`
fi

###################################################################################################### Define execution function:
run(){
	program="$1"
	library="$2"
	shift
	shift
	if [ -z "$library" ]
	then
		args=`echo -- "$@"`
	else
		args=`echo -l "$library" -- "$@"`
	fi
	echo "$program" "$@"
	for s in ${selected_systems[@]}
	do
		set +e
		set +o pipefail
		error_message=`"$script_dir/run-program.bash" -s "$s" -e "$program" $args 2>&1 1>/dev/null`
		error_status=$?
		set -e
		set -o pipefail
		if [ $error_status -eq 0 -a ! -z "$error_message" ]
		then
			error_status=1
		fi
		case $error_status in
			0) printf " $s";;	# all correct
			2) printf " -$s-";;	# configuration error for Scheme system => test skipped
			*)
				echo " !$s!"	# test failed (execution error) => print error and abort testing
				echo "$error_message"
				exit $error_status;;
		esac
	done
	echo ""
}

################################################################################################################## Execute tests:

# Test basic API:
for f in "$script_dir"/tests/*.scm
do
	run "$f" ""
done

# Test binary numbers example:
run "$script_dir/examples/binary-numbers/binary-numbers.scm" ""

# Test state machines example:
run "$script_dir/examples/state-machines/state-machines.scm" ""

# Test atomic Petri nets example:
for f in "$script_dir"/examples/atomic-petrinets/examples/*.scm
do
	run "$f" "$script_dir/examples/atomic-petrinets"
done

# Test composed Petri nets example (Guile is excluded because of issue #37):
for f in "$script_dir"/examples/composed-petrinets/examples/*.scm
do
	run "$f" "$script_dir/examples/composed-petrinets"
done

# Test fUML Activity Diagrams example:
for f in "$script_dir"/examples/ttc-2015-fuml-activity-diagrams/examples/contest-tests/*.ad
do
	input=${f%.ad}.adinput
	if [ ! -f "$input" ]
	then
		input=":false:"
	fi
	run "$script_dir/examples/ttc-2015-fuml-activity-diagrams/run.scm" "" "$f" "$input" 6 ":false:"
done

# Test SiPLE example:
for f in "$script_dir"/examples/siple/examples/correct/*.siple
do
	run "$script_dir/examples/siple/run.scm" "" "$f" ":false:"
done
for f in "$script_dir"/examples/siple/examples/incorrect/*.siple
do
	run "$script_dir/examples/siple/run.scm" "" "$f" ":true:"
done
