#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

selected_systems=()
unset abort_on_failed_test

################################################################################################################ Parse arguments:
while getopts s:xh opt
do
	case $opt in
		s)
			"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems+=( "$OPTARG" )
			;;
		x)
			abort_on_failed_test="false"
			;;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter). Permitted values:" >&2
			"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -i | \
				sed 's/^/             /' >&2
			echo "          If no system is selected, all installed and officially supported systems are tested." >&2
			echo "       -x Do not abort testing on error (multi-flag)." >&2
			echo "          By default, testing is aborted as soon as any test failed." >&2
			exit 64
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

if [[ ! -v "selected_systems[@]" ]]
then
	mapfile -t selected_systems < <(
		"$script_dir/../deploying/deployment-scripts/list-scheme-systems.bash" -i || kill -13 $$ )
fi

if [[ ! -v "abort_on_failed_test" ]]
then
	abort_on_failed_test="true"
fi

############################################################################################# Install RACR libraries as required:
printf "\033[1;33m%s\033[0m" "\
============================================= U P D A T E   I N S T A L L A T I O N =============================================

Ensure RACR library installations are up-to-date and install libraries as required. This may take a moment...
"

install_args=()
for s in "${selected_systems[@]}"
do
	install_args+=( -s )
	install_args+=( "$s" )
done
"$script_dir/../deploying/deployment-scripts/install.bash" "${install_args[@]}"

printf "\033[1;33m%s\033[0m" "
...Installation finished.
"

###################################################################################################### Define execution function:
declare -A excluded_systems
excluded_systems=()

tests_executed=0
tests_passed=0
tests_failed=0
tests_skipped=0

run(){
	measurements_dir="$script_dir/measurements/$measurements_dir"
	program="$1"
	if [ "$2" != "" ]
	then
		library=( -l "$2" )
	else
		library=()
	fi
	shift
	shift
	if (( $# != 0 ))
	then
		execution_arguments=( -- )
		execution_arguments+=( "$@" )
	else
		execution_arguments=()
	fi
	printf "\033[0;34m"
	echo "$program"
	i=1
	for a in "$@"
	do
		printf " %4i: %s\n" "$i" "$a"
		i=$(( i + 1 ))
	done
	printf "\033[0m"
	for s in "${selected_systems[@]}"
	do
		if [ "${excluded_systems["$s"]+x}" != "" ]
		then
			error_message=""
			error_status=64
		else
			mkdir -p "$measurements_dir"
			set +e
			set +o pipefail
			error_message="$( \
				"$script_dir/../profiling/profiling-scripts/measure.bash" \
					-c "$script_dir/profiling-configuration" \
					-t "$measurements_dir/profiling-table.txt" \
					-p "$script_dir/conduct-single-test.bash" \
					-- -e "$program" "${library[@]}" "${execution_arguments[@]}" \
					<<< "$s" \
					2>&1 1>/dev/null
			)"
			error_status=$?
			set -e
			set -o pipefail
		fi
		tests_executed=$(( tests_executed + 1 ))
		echo_trailing_newline="true"
		case $error_status in
			0) # all correct => test passed
				tests_passed=$(( tests_passed + 1 ))
				printf " \033[0;32m%s\033[0m" "$s"
				if [ "$error_message" != "" ]
				then
					printf "\n\n%s\n" "$error_message" >&2
					unset echo_trailing_newline
				fi
				;;
			64) # configuration error for Scheme system => test skipped
				tests_skipped=$(( tests_skipped + 1 ))
				printf " \033[1;33m-%s-\033[0m" "$s"
				if [ "$error_message" != "" ]
				then
					printf "\n\n%s\n" "$error_message" >&2
					unset echo_trailing_newline
				fi
				;;
			*) # execution error => test failed
				tests_failed=$(( tests_failed + 1 ))
				printf " \033[0;31m!%s!\033[0m" "$s"
				if [ "$error_message" != "" ]
				then
					printf "\n\n%s\n" "$error_message" >&2
					unset echo_trailing_newline
				fi
				if [ "$abort_on_failed_test" == "true" ] # abort testing if requested
				then
					echo " !!! ERROR: Testing aborted because of failed test !!!" >&2
					exit "$error_status"
				fi
				;;
		esac
	done
	if [[ -v "echo_trailing_newline" ]]
	then
		echo ""
	fi
}

################################################################################################################## Execute tests:
printf "\033[1;33m%s\033[0m" "
=================================================== S T A R T   T E S T I N G ===================================================

"

# Test basic API:
for f in "$script_dir"/*.scm
do
	excluded_systems=()
	for e in "$f.exclude."*
	do
		if [ -f "$e" ]
		then
			e="$( basename "$e" )"
			e="${e##*.}"
			excluded_systems["$e"]="$e"
		fi
	done
	measurements_dir="tests/$( basename "$f" )"
	run "$f" ""
done
excluded_systems=()

# Test binary numbers example:
measurements_dir="binary-numbers/binary-numbers.scm"
run "$script_dir/../examples/binary-numbers/binary-numbers.scm" ""

# Test state machines example:
measurements_dir="state-machines/state-machines.scm"
run "$script_dir/../examples/state-machines/state-machines.scm" ""

# Test atomic Petri nets example:
for f in "$script_dir"/../examples/atomic-petrinets/examples/*.scm
do
	measurements_dir="atomic-petrinets/$( basename "$f" )"
	run "$f" "$script_dir/../examples/atomic-petrinets"
done

# Test composed Petri nets example:
excluded_systems=( [guile]="guile" ) # Guile is excluded because of issue #37).
for f in "$script_dir"/../examples/composed-petrinets/examples/*.scm
do
	measurements_dir="composed-petrinets/$( basename "$f" )"
	run "$f" "$script_dir/../examples/composed-petrinets"
done
excluded_systems=()

# Test fUML Activity Diagrams example:
for f in "$script_dir"/../examples/ttc-2015-fuml-activity-diagrams/examples/contest-tests/*.ad
do
	input="${f%.ad}.adinput"
	if [ ! -f "$input" ]
	then
		input=":false:"
	fi
	measurements_dir="ttc-2015-fuml-activity-diagrams/$( basename "$f" )"
	run "$script_dir/../examples/ttc-2015-fuml-activity-diagrams/execute.scm" "" "$f" "$input" 6 ":true:" ":false:"
done

# Test SiPLE example:
for f in "$script_dir"/../examples/siple/examples/correct/*.siple
do
	measurements_dir="siple/correct/$( basename "$f" )"
	run "$script_dir/../examples/siple/execute.scm" "" "$f" ":false:"
done
for f in "$script_dir"/../examples/siple/examples/incorrect/*.siple
do
	measurements_dir="siple/incorrect/$( basename "$f" )"
	run "$script_dir/../examples/siple/execute.scm" "" "$f" ":true:"
done

############################################################################################################## Check performance:
printf "\033[1;33m%s\033[0m" "
=============================================== C H E C K   P E R F O R M A N C E ===============================================

"

######################################################################################################### Print summary and exit:
printf "\033[1;33m%s\033[0m" "
==================================================== T E S T   S U M M A R Y ====================================================

"

status_message="\
Number of tests: $tests_executed
Tests passed:    $tests_passed
Tests skipped:   $tests_skipped
Tests failed:    $tests_failed
"

if (( tests_failed > 0 ))
then
	printf "\033[0;31m%s\033[0m" "$status_message" >&2
	exit 1
elif (( tests_skipped > 0 ))
then
	printf "\033[1;33m%s\033[0m" "$status_message"
	exit 0
fi
printf "\033[0;32m%s\033[0m" "$status_message"
exit 0
