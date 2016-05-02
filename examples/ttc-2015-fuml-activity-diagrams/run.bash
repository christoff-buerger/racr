#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if [ $# -eq 0 ]
then
	"$script_dir/run.bash" -h
	exit $?
fi
while getopts xs:d:i:m:h opt
do
	case $opt in
		x)
			print_trace=":false:";;
		s)
			selected_system=`echo $selected_system -s "$OPTARG"`;;
		d)
			if [ -z ${diagram+x} ]
			then
				diagram="$OPTARG"
			else
				echo " !!! ERROR: Several activity diagrams for execution selected via -d flag !!!" >&2
				exit 2
			fi;;
		i)
			if [ -z ${input+x} ]
			then
				input="$OPTARG"
			else
				echo " !!! ERROR: Several diagram inputs for execution selected via -i flag !!!" >&2
				exit 2
			fi;;
		m)
			if [ -z ${mode+x} ]
			then
				mode="$OPTARG"
			else
				echo " !!! ERROR: Several modes for diagram execution selected via -m flag !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -s Scheme system (optional parameter). Permitted values:" >&2
			echo "`"$script_dir/../../list-scheme-systems.bash" -i | sed 's/^/             /'`" >&2
			echo "          By default, Larceny is used." >&2
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
			echo "       -x Deactivate printing the execution trace on stdout (optional multi-parameter)." >&2
			echo "          By default, the execution trace is printed." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${print_trace+x} ]
then
	print_trace=":true:"
fi

if [ -z ${selected_system+x} ]
then
	selected_system=`echo -s "larceny"`
fi

if [ -z ${diagram+x} ]
then
	echo " !!! ERROR: No activity diagram to execute given via -d flag !!!" >&2
	exit 2
fi

if [ -z ${input+x} ]
then
	input=":false:"
fi

if [ -z ${mode+x} ]
then
	mode=6
elif (( "$mode" < 1 || "$mode" > 7 ))
then
	echo " !!! ERROR: No valid execution mode selected !!!" >&2
	exit 2
fi

####################################################################################################### Execute activity diagram:
"$script_dir/../../run-program.bash" $selected_system -e "$script_dir/run.scm" -- "$diagram" "$input" "$mode" "$print_trace"
