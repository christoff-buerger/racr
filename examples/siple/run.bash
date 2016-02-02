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
			selected_system=`echo $selected_system -s "$OPTARG"`;;
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
			echo "       -x Specifies, that the executed program is not a valid SiPLE program and runtime" >&2
			echo "          errors are expected. If no runtime error is encountered throughout interpretation," >&2
			echo "          an error message is printed on stderr." >&2
			echo "          By default, a correct SiPLE program is expected." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z ${execute_incorrect+x} ]
then
	execute_incorrect=":false:"
fi

if [ -z ${selected_system+x} ]
then
	selected_system=`echo -s "larceny"`
fi

if [ -z ${program+x} ]
then
	echo " !!! ERROR: No SiPLE program to interpret given via -e flag !!!" >&2
	exit 2
fi

########################################################################################################## Execute SiPLE program:
"$script_dir/../../run-program.bash" $selected_system -e "$script_dir/run.scm" "$program" "$execute_incorrect"
