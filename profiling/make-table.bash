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
	"$script_dir/make-table.bash" -h
	exit $?
fi
while getopts c:p:t:h opt
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
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -p Input pipe providing measurement results to log (mandatory parameter)." >&2
			echo "       -t Measurements table used for logging (mandatory parameter)." >&2
			echo "          Created if not existent. New measurements are appended." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${profiling_configuration+x} ] || [ ! -f "$profiling_configuration" ]
then
	echo " !!! ERROR: Non-existing or no profiling configuration specified via -c flag !!!" >&2
	exit 2
fi

if [ -z ${measurements_table+x} ] ||
   [ ! -d "`dirname "$measurements_table"`" ] ||
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
. "$script_dir/parse-profiling-configuration.bash" # Sourced script sets configuration!

############################################################################################################# Print table header:
if [ ! -e "$measurements_table" ]
then
	for (( i = 0; i < number_of_parameters; i++ ))
	do
		printf " %-18s |" "${parameter_names[$i]}" >> "$measurements_table"
	done
	for (( i = 0; i < number_of_results; i++ ))
	do
		printf "| %-18s" "${result_names[$i]}" >> "$measurements_table"
	done
	printf "\n" >> "$measurements_table"
	for (( i = 0; i < number_of_parameters; i++ ))
	do
		printf "%s" "--------------------+" >> "$measurements_table"
	done
	for (( i = 0; i < number_of_results; i++ ))
	do
		printf "%s" "+--------------------" >> "$measurements_table"
	done
	printf "\n" >> "$measurements_table"
fi

############################################################################################################ Print table content:
column_count=0
while true #lsof "$measurements_pipe"
do
	if read -r line
	then
		if (( column_count < number_of_parameters ))
		then
			printf " %-18s |" "$line" >> "$measurements_table"
		elif (( column_count < number_of_parameters + number_of_results ))
		then
			printf "| %-18s" "$line" >> "$measurements_table"
		fi
		column_count=$(( column_count + 1 ))
		if (( column_count >= number_of_parameters + number_of_results ))
		then
			print "\n" >> "$measurements_table"
			column_count=0
		fi
	else
		break
	fi
done < "$measurements_pipe"
