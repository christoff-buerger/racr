#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
while getopts s:xh opt
do
	case $opt in
		s)
			"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems+=( "$OPTARG" );;
		x)
			abort_on_failed_test="false";;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter). Permitted values:" >&2
			echo "`"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -i | \
				sed 's/^/             /'`" >&2
			echo "          If no system is selected, all installed and officially supported systems are tested." >&2
			echo "       -x Do not abort testing on error (multi-flag)." >&2
			echo "          By default, testing is aborted as soon as any test failed." >&2
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
	selected_systems=`"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -i`
fi

if [ -z ${abort_on_failed_test+x} ]
then
	abort_on_failed_test="true"
fi

###################################################################################################### Define execution function:
tests_executed=0
tests_passed=0
tests_failed=0
tests_skipped=0

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
		error_message=`"$script_dir/../deploying/deployment-scripts/execute.bash" \
			-s "$s" -e "$program" $args 2>&1 1>/dev/null`
		error_status=$?
		set -e
		set -o pipefail
		tests_executed=$(( tests_executed + 1 ))
		if [ $error_status -eq 0 -a ! -z "$error_message" ]
		then
			error_status=1
		fi
		case $error_status in
			0) # all correct => test passed
				printf " $s"
				tests_passed=$(( tests_passed + 1));;
			2) # configuration error for Scheme system => test skipped
				printf " -$s-"
				tests_skipped=$(( tests_skipped + 1));;
			*) # test failed (execution error) => print error and...
				echo " !$s!"
				echo "$error_message" >&2
				if [[ "$abort_on_failed_test" =~ "true" ]] # ...abort testing if requested
				then
					echo " !!! ERROR: Testing aborted because of failed test !!!" >&2
					exit $error_status
				fi
				tests_failed=$(( tests_failed + 1 ));;
		esac
	done
	echo ""
}

################################################################################################################## Execute tests:

# Test basic API:
for f in "$script_dir"/*.scm
do
	run "$f" ""
done

# Test binary numbers example:
run "$script_dir/../examples/binary-numbers/binary-numbers.scm" ""

# Test state machines example:
run "$script_dir/../examples/state-machines/state-machines.scm" ""

# Test atomic Petri nets example:
for f in "$script_dir"/../examples/atomic-petrinets/examples/*.scm
do
	run "$f" "$script_dir/../examples/atomic-petrinets"
done

# Test composed Petri nets example (Guile is excluded because of issue #37):
for f in "$script_dir"/../examples/composed-petrinets/examples/*.scm
do
	run "$f" "$script_dir/../examples/composed-petrinets"
done

# Test fUML Activity Diagrams example:
for f in "$script_dir"/../examples/ttc-2015-fuml-activity-diagrams/examples/contest-tests/*.ad
do
	input=${f%.ad}.adinput
	if [ ! -f "$input" ]
	then
		input=":false:"
	fi
	run "$script_dir/../examples/ttc-2015-fuml-activity-diagrams/run.scm" "" "$f" "$input" 6 ":true:" ":false:"
done

# Test SiPLE example:
for f in "$script_dir"/../examples/siple/examples/correct/*.siple
do
	run "$script_dir/../examples/siple/run.scm" "" "$f" ":false:"
done
for f in "$script_dir"/../examples/siple/examples/incorrect/*.siple
do
	run "$script_dir/../examples/siple/run.scm" "" "$f" ":true:"
done

status_message="=====T=E=S=T===S=U=M=M=A=R=Y=====
Number of tests: $tests_executed
Tests passed:    $tests_passed
Tests skipped:   $tests_skipped
Tests failed:    $tests_failed"
if [ $tests_failed -gt 0 ]
then
	printf "%s\n" "$status_message" >&2
	exit 2
fi
printf "%s\n" "$status_message"
