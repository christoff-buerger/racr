#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

while getopts s:d:i:m: opt
do
	case $opt in
		s)
			if [ -z ${selected_system+x} ]
			then
				selected_system="$OPTARG"
			else
				echo " !!! ERROR: Several Scheme systems for execution selected via -s flag !!!" >&2
				exit 2
			fi;;
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
		?)
			echo "Usage: -s Scheme system (by default larceny)." >&2
			echo "       -d Activity diagram." >&2
			echo "       -i Activity diagram input." >&2
			echo "       -m Mode (1=parsing, 2=AD-well-formedness, 3=PN-generation, 4=PN-well-formedness" >&2
			echo "                5=PN-execution (no enabled passes), 6=PN-execution (use enabled passes))." >&2
			echo "          The default is 5: Petri net execution, one transition each step." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z ${selected_system+x} ]
then
	selected_system="larceny"
fi

if [ -z ${diagram+x} ]
then
	echo " !!! ERROR: No activity diagram to interpret given via -d flag !!!" >&2
	exit 2
fi

if [ -z ${input+x} ]
then
	input=":false:"
fi

if [ -z ${mode+x} ]
then
	mode=5
else if (( "$mode" < 1 || "$mode" > 6 ))
then
	echo " !!! ERROR: No valid mode selected !!!" >&2
	exit 2
fi fi

############################################################################### Configure temporary resources & execution script:
my_exit(){
	rm "$script_dir/start-script.scm"
	exit 0
}
trap 'my_exit' 1 2 3 9 15

echo "#!r6rs" > "$script_dir/start-script.scm"
echo "(import (rnrs) (ttc-2015-fuml-activity-diagrams user-interface))" >> "$script_dir/start-script.scm"
echo "(define diagram (cadr (command-line)))" >> "$script_dir/start-script.scm"
echo "(define input (caddr (command-line)))" >> "$script_dir/start-script.scm"
echo "(define mode (cadddr (command-line)))" >> "$script_dir/start-script.scm"
echo '(set! input (if (string=? input ":false:") #f input))' >> "$script_dir/start-script.scm"
echo '(set! mode (string->number mode))' >> "$script_dir/start-script.scm"
echo "(run-activity-diagram diagram input mode)" >> "$script_dir/start-script.scm"

####################################################################################################### Execute activity diagram:
"$script_dir/../../run-program.bash" -s "$selected_system" -e "$script_dir/start-script.scm" "$diagram" "$input" "$mode"
my_exit
