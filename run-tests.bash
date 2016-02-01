#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
known_systems=( racket guile larceny petite )
selected_systems=()

while getopts s: opt
do
	case $opt in
		s)
			if [[ " ${known_systems[@]} " =~ " ${OPTARG} " ]]
			then
				if which "${OPTARG}" > /dev/null
				then
					selected_systems+=( "$OPTARG" )
				else
					echo " !!! ERROR: Scheme system [$OPTARG] not installed !!!" >&2
					exit 2
				fi
			else
				echo " !!! ERROR: Unknown [$OPTARG] Scheme system !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (${known_systems[@]})" >&2
			echo "          Several systems can be set. If no system is selected, all supported are tested." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$selected_systems" ]
then
	for s in ${known_systems[@]}
	do
		if which "$s" > /dev/null
		then
			selected_systems+=( "$s" )
		fi
	done
	if [ -z "$selected_systems" ]
	then
		echo " !!! ERROR: No Scheme system found !!!" >&2
		exit 2
	fi
fi

###################################################################################################### Define execution function:
run(){
	program="$1"
	library="$2"
	shift
	shift
	args=`if [ -z "$library" ]; then echo $*; else echo -l "$library" $*; fi`
	echo "$program" $*
	if [ -z "$library" ]
	then
		supported_systems=${selected_systems[@]}
	else
		configuration_to_parse="$library/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
	fi
	for s in ${selected_systems[@]}
	do
		if [[ " ${supported_systems[@]} " =~ "$s" ]]
		then
			printf " $s"
			"$script_dir/run-program.bash" -s "$s" -e "$program" $args
		fi
	done
	echo ""
}

################################################################################################################## Execute tests:
echo "=========================================>>> Run Tests:"

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
	run "$script_dir/examples/ttc-2015-fuml-activity-diagrams/run.scm" "" "$f" "$input" 5 ":false:"
done

# Test SiPLE example:
for f in "$script_dir"/examples/siple/examples/correct/*.siple
do
	run "$script_dir/examples/siple/run-correct.scm" "" "$f"
done
for f in "$script_dir"/examples/siple/examples/incorrect/*.siple
do
	run "$script_dir/examples/siple/run-incorrect.scm" "" "$f"
done

# Test Tiny C++ example:
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "Tiny C++ Racket:"
	"$script_dir/profiling/tinycpp/examples/run-examples.bash" Racket
fi
if [[ " ${selected_systems[@]} " =~ "guile" ]]
then
	echo "Tiny C++ Guile:"
	"$script_dir/profiling/tinycpp/examples/run-examples.bash" Guile
fi
if [[ " ${selected_systems[@]} " =~ "larceny" ]]
then
	echo "Tiny C++ Larceny:"
	"$script_dir/profiling/tinycpp/examples/run-examples.bash" Larceny
fi
if [[ " ${selected_systems[@]} " =~ "petite" ]]
then
	echo "Tiny C++ Petite Chez Scheme:"
	"$script_dir/profiling/tinycpp/examples/run-examples.bash" Petite
fi
