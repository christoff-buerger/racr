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
	"$script_dir/record.bash" -h
	exit $?
fi

while getopts c:p:t:xh opt
do
	case $opt in
		c)
			if [ -z ${profiling_configuration+x} ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c flag !!!" >&2
				exit 2
			fi;;
		p)
			if [ -z ${measurements_pipe+x} ]
			then
				measurements_pipe="$OPTARG"
			else
				echo " !!! ERROR: Several input pipes selected via -p flag !!!" >&2
				exit 2
			fi;;
		t)
			if [ -z ${measurements_table+x} ]
			then
				measurements_table="$OPTARG"
			else
				echo " !!! ERROR: Several measurement tables selected via -t flag !!!" >&2
				exit 2
			fi;;
		x)
			test_run=true;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -p Input pipe providing measurement results to record (mandatory parameter)." >&2
			echo "       -t Measurements table used for recording (mandatory parameter)." >&2
			echo "          Created if not existent. New measurements are appended." >&2
			echo "       -x Only test arguments (optional multi-flag)." >&2
			echo "          No measurements are recorded. The measurements table is not updated." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$@] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${profiling_configuration+x} ] || [ ! -f "$profiling_configuration" ]
then
	echo " !!! ERROR: Non-existing or no profiling configuration specified via -c flag !!!" >&2
	exit 2
fi

if [ -z "$measurements_table" ] ||
   [ -e "$measurements_table" -a ! -f "$measurements_table" ]
then
	echo " !!! ERROR: Invalid or no measurements table specified via -t flag !!!" >&2
	exit 2
fi

if [ -z ${measurements_pipe+x} ] || [ ! -p "$measurements_pipe" ]
then
	echo " !!! ERROR: Non-existing or no input pipe specified via -p flag !!!" >&2
	exit 2
fi

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!
if [ ! -z "$test_run" ]
then
	exit 0
fi

############################################################################################################# Print table header:
mkdir -p "`dirname "$measurements_table"`"
if [ ! -e "$measurements_table" ]
then
	i=0
	for (( ; i < number_of_criteria - 1; i++ ))
	do
		printf " %-19s |" "${criteria_names[$i]}" >> "$measurements_table"
	done
	printf " %-19s \n" "${criteria_names[$i]}" >> "$measurements_table"
	for (( i = 1; i < number_of_criteria; i++ ))
	do
		printf "%s" "---------------------+" >> "$measurements_table"
	done
	echo "---------------------" >> "$measurements_table"
fi

############################################################################################################ Print table content:
column_count=1
while true #lsof "$measurements_pipe"
do
	if read -r line
	then
		if (( column_count < number_of_criteria ))
		then
			printf " %19s |" "$line" >> "$measurements_table"
			column_count=$(( column_count + 1 ))
		else
			printf " %19s \n" "$line" >> "$measurements_table"
			column_count=1
		fi
	else
		if (( column_count > 1 ))
		then # fix table if entries are missing
			for (( i = column_count; i < number_of_criteria; i++ ))
			do
				if (( i == number_of_parameters + 1 ))
				then
					printf "             aborted |" >> "$measurements_table"
				else
					printf " ------------------- |" >> "$measurements_table"
				fi
			done
			if (( column_count <= number_of_criteria ))
			then
				echo " ------------------- " >> "$measurements_table"
			fi
		fi
		break
	fi
done < "$measurements_pipe"
