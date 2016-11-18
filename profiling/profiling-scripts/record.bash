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
			echo "       -p Input pipe providing measurement results to record (mandatory parameter)." >&2
			echo "       -t Measurements table used for recording (mandatory parameter)." >&2
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
. "$script_dir/configure.bash" # Sourced script sets configuration!

############################################################################################################# Print table header:
if [ ! -e "$measurements_table" ]
then
	for (( i = 0; i < number_of_parameters; i++ ))
	do
		printf " %-19s |" "${parameter_names[$i]}" >> "$measurements_table"
	done
	printf "| %-19s " "Measurement date" >> "$measurements_table"
	printf "| %-5s |" "Error" >> "$measurements_table"
	for (( i = 0; i < number_of_results; i++ ))
	do
		printf "| %-19s " "${result_names[$i]}" >> "$measurements_table"
	done
	printf "\n" >> "$measurements_table"
	for (( i = 0; i < number_of_parameters; i++ ))
	do
		printf "%s" "---------------------+" >> "$measurements_table"
	done
	printf "%s" "+---------------------+-------+" >> "$measurements_table"
	for (( i = 0; i < number_of_results; i++ ))
	do
		printf "%s" "+---------------------" >> "$measurements_table"
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
			printf " %19s |" "$line" >> "$measurements_table"
		elif (( column_count == number_of_parameters ))
		then
			printf "| %19s " "$line" >> "$measurements_table"
		elif (( column_count == number_of_parameters + 1 ))
		then
			printf "|   %1s   |" "$line" >> "$measurements_table"
		elif (( column_count < number_of_parameters + number_of_results + 2 ))
		then
			printf "| %19s " "$line" >> "$measurements_table"
		fi
		column_count=$(( column_count + 1 ))
		if (( column_count >= number_of_parameters + number_of_results + 2 ))
		then
			printf "\n" >> "$measurements_table"
			column_count=0
		fi
	else
		if [ $column_count -gt 0 ]
		then # fix table if entries are missing
			for (( i = column_count; i < number_of_parameters; i++ ))
			do
				printf " ------------------- |" >> "$measurements_table"
			done
			if (( column_count <= number_of_parameters ))
			then
				printf "| ------------------- " >> "$measurements_table"
			elif (( column_count <= number_of_parameters + 1 ))
			then
				printf "|   A   |" >> "$measurements_table"
			fi
			if (( column_count >= number_of_parameters + 2 ))
			then
				for (( i = column_count - number_of_parameters - 2; i < number_of_results; i++ ))
				do
					printf "| ------------------- " >> "$measurements_table"
				done
			else
				for (( i = 0; i < number_of_results; i++ ))
				do
					printf "| ------------------- " >> "$measurements_table"
				done
			fi
			printf "\n" >> "$measurements_table"
		fi
		break
	fi
done < "$measurements_pipe"
