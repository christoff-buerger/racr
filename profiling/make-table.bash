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
while getopts c:t:p: opt
do
	case $opt in
		c)
			if [ -z ${configuration+x} ]
			then
				configuration="$OPTARG"
			else
				echo " !!! ERROR: Several measurement configurations selected via -c flag !!!" >&2
				exit 2
			fi;;
		t)
			if [ -z ${table_file+x} ]
			then
				table_file="$OPTARG"
			else
				echo " !!! ERROR: Several table files selected via -t flag !!!" >&2
				exit 2
			fi;;
		p)
			if [ -z ${input_pipe+x} ]
			then
				input_pipe="$OPTARG"
			else
				echo " !!! ERROR: Several input pipes selected via -p flag !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -c Measurement configuration (mandatory parameter)." >&2
			echo "       -t Table file logging inputs (mandatory parameter)." >&2
			echo "          Created if not existent, otherwise inputs are appended." >&2
			echo "       -p Named input pipe (mandatory parameter)." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${configuration+x} ] || [ ! -f "$configuration" ]
then
	echo " !!! ERROR: Non-existing or no measurement configuration specified via -c flag !!!" >&2
	exit 2
fi

if [ -z ${table_file+x} ]
then
	echo " !!! ERROR: No table file for logging specified via -t flag !!!" >&2
	exit 2
fi

if [ -z ${input_pipe+x} ] || [ ! -p "$input_pipe" ]
then
	echo " !!! ERROR: Non-existing or no input pipe specified via -p flag !!!" >&2
	exit 2
fi

############################################################################################################# Read configuration:
while read -r line
do
	IFS='/' read -ra config_line <<< "$line"
	column_names+=( "${config_line[0]}" )
done < "$configuration"
num_columns=${#column_names[@]}

############################################################################################################# Print table header:
if [ ! -e "$table_file" ]
then
	for (( i = 0; i < num_columns; i++ ))
	do
		printf " %-18s |" "${column_names[$i]}" >> "$table_file"
	done
	printf " %-18s \n" "Time in s" >> "$table_file"
	for (( i = 0; i < num_columns; i++ ))
	do
		printf "%s" "--------------------+" >> "$table_file"
	done
	echo "--------------------" >> "$table_file"
fi

############################################################################################################ Print table content:
column_count=0
while true #lsof "$input_pipe"
do
	if read -r line
	then
		if (( column_count < num_columns ))
		then
			printf " %-18s |" "$line" >> "$table_file"
			column_count=$(( column_count + 1 ))
		else
			printf " %-18s \n" "$line" >> "$table_file"
			column_count=0
		fi
	else
		break
	fi
done < "$input_pipe"
