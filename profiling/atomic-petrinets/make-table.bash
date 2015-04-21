#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

########################################################################################### Parse arguments & read configuration:
while getopts t:c: opt
do
	case $opt in
		t)	table_file="$OPTARG";;
		c)	configuration="$OPTARG";;
		?)
			echo "Usage: -t Table file (created if non-existent, otherwise appended)"
			echo "       -c Measurement configuration"
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$table_file" ]
then
	echo " !!! ERROR: No table file given !!!" >& 2
	exit 2
fi
if [ -z "$configuration" ]
then
	echo " !!! ERROR: No measurement configuration given !!!" >& 2
	exit 2
fi

while read -u3 line
do
	IFS='/' read -ra config_line <<< "$line"
	column_names+=( "${config_line[0]}" )
done 3< $configuration

############################################################################################################# Print table header:
num_columns=${#column_names[@]}
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
#while read line
#do
	#
#done
