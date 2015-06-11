#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

########################################################################################### Parse arguments & read configuration:
while getopts t:c:p: opt
do
	case $opt in
		t)	table_file="$OPTARG";;
		c)	configuration="$OPTARG";;
		p)	input_pipe="$OPTARG";;
		?)
			echo "Usage: -t Table file (created if not existent, otherwise input is appended)"
			echo "       -c Measurement configuration"
			echo "       -p Named input pipe"
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$table_file" ]
then
	echo " !!! ERROR: No table file given !!!" >&2
	exit 2
fi
if [ -z "$configuration" ]
then
	echo " !!! ERROR: No measurement configuration given !!!" >&2
	exit 2
fi
if [ -z "$input_pipe" ]
then
	echo " !!! ERROR: No input pipe given !!!" >&2
	exit 2
fi

while read -r line
do
	IFS='/' read -ra config_line <<< "$line"
	column_names+=( "${config_line[0]}" )
done < $configuration
num_columns=${#column_names[@]}

############################################################################################################# Print table header:
if [ ! -e $table_file ]
then
	for (( i = 0; i < num_columns; i++ ))
	do
		printf " %-18s |" ${column_names[$i]} >> $table_file
	done
	printf " %-18s \n" "Time in s" >> $table_file
	for (( i = 0; i < num_columns; i++ ))
	do
		printf "%s" "--------------------+" >> $table_file
	done
	echo "--------------------" >> $table_file
fi

############################################################################################################ Print table content:
column_count=0
while true #lsof "$input_pipe"
do
	if read -r line
	then
		if (( column_count < num_columns ))
		then
			printf " %-18s |" "$line" >> $table_file
			column_count=$(( column_count + 1 ))
		else
			printf " %-18s \n" "$line" >> $table_file
			column_count=0
		fi
	else
		break
	fi
done < $input_pipe
