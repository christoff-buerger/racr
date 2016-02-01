#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

while getopts xs:e: opt
do
	case $opt in
		x)
			if [ -z ${execute_incorrect+x} ]
			then
				execute_incorrect=":true:"
			else
				echo " !!! ERROR: Incorrect execution several times set via -x flag !!!" >&2
				exit 2
			fi;;
		s)
			if [ -z ${selected_system+x} ]
			then
				selected_system="$OPTARG"
			else
				echo " !!! ERROR: Several Scheme systems for execution selected via -s flag !!!" >&2
				exit 2
			fi;;
		e)
			if [ -z ${program+x} ]
			then
				program="$OPTARG"
			else
				echo " !!! ERROR: Several SiPLE programs for execution selected via -e flag !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (by default larceny)." >&2
			echo "       -e SiPLE program to interpret." >&2
			echo "       -x The executed program is not a valid SiPLE program; expect runtime errors." >&2
			echo "          Throw an error, if no runtime error is encountered throughout interpretation." >&2
			echo "          By default, the -x flag is not set; a correct SiPLE program is expected." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z ${selected_system+x} ]
then
	selected_system="larceny"
fi

if [ -z ${program+x} ]
then
	echo " !!! ERROR: No SiPLE program to interpret given via -e flag !!!" >&2
	exit 2
fi

############################################################################### Configure temporary resources & execution script:
my_exit(){
	rm "$script_dir/start-script.scm"
	exit 0
}
trap 'my_exit' 1 2 3 9 15

if [ -z ${execute_incorrect+x} ]
then
	echo "#!r6rs" > "$script_dir/start-script.scm"
	echo "(import (rnrs) (siple main))" >> "$script_dir/start-script.scm"
	echo "(siple-interpret (cadr (command-line)))" >> "$script_dir/start-script.scm"
else
	echo "#!r6rs" > "$script_dir/start-script.scm"
	echo "(import (rnrs) (siple main) (siple exception-api) (racr testing))" >> "$script_dir/start-script.scm"
	echo "(assert-exception siple-exception? (siple-interpret (cadr (command-line))))" >> "$script_dir/start-script.scm"
fi

########################################################################################################## Execute SiPLE program:
"$script_dir/../../run-program.bash" -s "$selected_system" -e "$script_dir/start-script.scm" "$program"
my_exit
