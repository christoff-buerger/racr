#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if (( $# == 0 ))
then
	"$script_dir/record.bash" -h
	exit $?
fi

while getopts c:p:t:xh opt
do
	case $opt in
		c)
			if [[ ! -v "profiling_configuration" ]]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi
			;;
		p)
			if [[ ! -v "measurements_pipe" ]]
			then
				measurements_pipe="$OPTARG"
			else
				echo " !!! ERROR: Several input pipes selected via -p parameter !!!" >&2
				exit 2
			fi
			;;
		t)
			if [[ ! -v "measurements_table" ]]
			then
				measurements_table="$OPTARG"
			else
				echo " !!! ERROR: Several measurements tables selected via -t parameter !!!" >&2
				exit 2
			fi
			;;
		x)
			test_run=true
			;;
		h|*)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -p Input pipe providing measurement results to record (mandatory parameter if no -x)." >&2
			echo "          If the -x flag is not set, the measurement results provided via the pipe are" >&2
			echo "          processed by an asynchronous process whose PID is echoed. Callers can use this" >&2
			echo "          PID to wait for the recording of piped measurements to finish and thereby avoid" >&2
			echo "          too early termination or premature deletion of the pipe." >&2
			echo "       -t Measurements table used for recording (mandatory parameter)." >&2
			echo "          Created if not existent. New measurements are appended." >&2
			echo "       -x Only test arguments (optional multi-flag)." >&2
			echo "          No measurements are recorded; no asynchronous recording process is started" >&2
			echo "          and therefore no PID of such echoed." >&2
			echo "          The measurements table is not updated." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if (( $# != 0 ))
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [[ ! -v "profiling_configuration" ]] || [[ ! -f "$profiling_configuration" ]]
then
	echo " !!! ERROR: Non-existing or no profiling configuration specified via -c parameter !!!" >&2
	exit 2
fi

if [[ "$test_run" != "" ]] && [[ ! -v "measurements_pipe" ]]
then
	:
elif [[ ! -v "measurements_pipe" ]] || [[ ! -p "$measurements_pipe" ]]
then
	echo " !!! ERROR: Non-existing or no input pipe specified via -p parameter !!!" >&2
	exit 2
fi

if [[ "$measurements_table" == "" ]] || { [[ -e "$measurements_table" ]] && [[ ! -f "$measurements_table" ]]; }
then
	echo " !!! ERROR: Invalid or no measurements table specified via -t parameter !!!" >&2
	exit 2
fi

##################################################################################### Configure temporary and external resources:
measurements_table_lock=""

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release lock on measurements table:
	if [[ -f "$measurements_table_lock" ]]
	then
		"$measurements_table_lock"
	fi
	# Return captured exit status (i.e., if the original script execution succeeded or not):	
	exit "$exit_status"
}
trap 'my_exit' 0 1 2 3 15

mkdir -p "$( dirname "$measurements_table" )" # Parent directory required for locking => always create.

if [[ "$test_run" != "" ]]
then
	measurements_table_lock="$( \
		"$script_dir/../../deploying/deployment-scripts/lock-files.bash" \
		-x " !!! ERROR: Failed to check measurements table [$measurements_table]; table subject to recording !!!" \
		-k "measurements table read lock" \
		-- "$measurements_table" )"
else
	measurements_table_lock="$( \
		"$script_dir/../../deploying/deployment-scripts/lock-files.bash" \
		-x " !!! ERROR: Failed to record measurements; measurements table [$measurements_table] already in use !!!" \
		-- "$measurements_table" )"
fi

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

i=0
for (( ; i < number_of_criteria - 1; i++ ))
do
	header[0]+="$( printf " %-20s |" "${criteria_names[$i]}" )"
done
header[0]+="$( printf " %-20s " "${criteria_names[$i]}" )"
for (( i = 1; i < number_of_criteria; i++ ))
do
	header[1]+="$( printf "%s" "----------------------+" )"
done
header[1]+="$( printf "%s" "----------------------" )"

############################################################################################## Check existing measurements table:
if [[ -e "$measurements_table" ]]
then
	i=0
	for h in "${header[@]}"
	do
		if IFS='' read -r line
		then
			if [[ "$line" != "$h" ]]
			then
				break
			fi
		else
			break
		fi
		i=$(( i + 1 ))
	done < "$measurements_table"
	if (( "$i" != ${#header[@]} ))
	then
		echo " !!! ERROR: Measurements table [$measurements_table] has malformed header !!!" >&2
		exit 2 # triggers 'my_exit'
	fi
	
	line_number=3
	while IFS='' read -r line
	do
		cell_number=0
		IFS='|' read -r -a cells <<< "$line"
		for cell in "${cells[@]}"
		do
			if (( ${#cell} != 22 )) || [[ "$cell" =~ "\t" ]]
			then
				break
			fi
			cell_number=$(( cell_number + 1 ))
		done
		if (( "$cell_number" != "$number_of_criteria" ))
		then
			printf " !!! ERROR: Measurements table [%s] has malformed content " "$measurements_table" >&2
			echo   "(line $line_number, cell $(( cell_number + 1 ))) !!!" >&2
			exit 2 # triggers 'my_exit'
		fi
		line_number=$(( line_number + 1 ))
	done < <( tail -n +3 "$measurements_table" )
fi

if [[ "$test_run" != "" ]]
then
	exit 0 # triggers 'my_exit'
fi

############################################################################################################# Print table header:
if [[ ! -e "$measurements_table" ]]
then
	for h in "${header[@]}"
	do
		echo "$h" >> "$measurements_table"
	done
fi

############################################################################################################ Print table content:
record(){
	trap 'my_exit' 0 1 2 3 15
	column_count=1
	while true
	do
		if IFS='' read -r cell
		then
			if (( column_count < number_of_criteria ))
			then
				cell_separator="|"
				column_count=$(( column_count + 1 ))
			else
				cell_separator=$'\n'
				column_count=1
			fi
			if (( ${#cell} == 22 )) && [[ "$cell" =~ ^"<".*">"$|^" ".*" "$ ]] && [[ ! "$cell" =~ "\t"|"|" ]]
			then
				printf "%s%s" "$cell" "$cell_separator" >> "$measurements_table"
			elif (( ${#cell} > 20 )) || [[ "$cell" =~ "\t"|"|" ]]
			then
				printf "<-------------------->%s" "$cell_separator" >> "$measurements_table"
			else
				printf " %20s %s" "$cell" "$cell_separator" >> "$measurements_table"
			fi
		else
			if (( column_count > 1 ))
			then # fix table if entries are missing
				for (( i = column_count; i < number_of_criteria; i++ ))
				do
					if (( i == number_of_parameters + 1 ))
					then
						printf "              aborted |" >> "$measurements_table"
					else
						printf "<????????????????????>|" >> "$measurements_table"
					fi
				done
				if (( column_count <= number_of_criteria ))
				then
					echo "<????????????????????>" >> "$measurements_table"
				fi
			fi
			break
		fi
	done < "$measurements_pipe"
	exit 0 # triggers 'my_exit'
}

trap - 0 1 2 3 15 # Hand over the [my_exit] routine for cleaning up the measurements table lock to...
record >/dev/null & # ...the asynchronous process recording the measurements and...
echo $! # ...return its PID.
exit 0
