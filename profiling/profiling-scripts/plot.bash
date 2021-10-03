#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/plot.bash" -h
	exit $?
fi

declare -A criteria
criteria=()

while getopts c:l:x:y:z:h opt
do
	case $opt in
		c)
			if [ -z ${profiling_configuration+x} ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi;;
		l|x|y|z)
			if [ -z ${criteria["$opt"]+x} ]
			then
				criteria["$opt"]="$OPTARG"
			else
				echo " !!! ERROR: Several measurement criteria for -$opt parameter selected !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -l Measurement criteria used for plot labels (mandatory parameter)." >&2
			echo "       -x Measurement criteria used for x-axis coordinates (mandatory parameter)." >&2
			echo "       -y Measurement criteria used for y-axis coordinates (mandatory parameter)." >&2
			echo "       -z Measurement criteria used for z-axis coordinates (optional parameter)." >&2
			echo "       -- List of source tables to plot (mandatory parameter)." >&2
			echo "          Must be non-empty." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

for c in l x y
do
	if [ -z "${criteria["$c"]}" ]
	then
		echo " !!! ERROR: No measurement criteria for -$c parameter selected !!!" >&2
		exit 2
	fi
done

for a in "$@"
do
	if [ ! -f "$a" ]
	then
		echo " !!! ERROR: Non-existing source table [$a] specified via '--' argument list !!!" >&2
		exit 2
	fi
	source_tables+=( "$a" )
	shift
done
if [ -z ${source_tables+x} ]
then
	echo " !!! ERROR: No source table specified via '--' argument list !!!" >&2
	exit 2
fi

##################################################################################### Configure temporary and external resources:
tmp_dir=""

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Delete all temporary resources:
	rm -rf "$tmp_dir"
	# Return captured exit status (i.e., if the original script execution succeeded or not):	
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

tmp_dir="$( "$script_dir/../../deploying/deployment-scripts/create-temporary.bash" -t d )"
#plotting_pipe="$tmp_dir/plotting-pipe.fifo"
#plotting_script="$tmp_dir/plotting-script.r"

#mkfifo "$plotting_pipe"

"$script_dir/check-tables.bash" -c "$profiling_configuration" -- "${source_tables[@]}"

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

# Find l, x, y, z measurement criteria in profiling configuration and store their index for later use:
declare -A criteria_index
criteria_index=()
for c in "${!criteria[@]}"
do
	i=0
	for n in "${criteria_names[@]}"
	do
		if [ "$n" == "${criteria["$c"]}" ]
		then
			break
		fi
		i=$(( i + 1 ))
	done
	if (( i == number_of_criteria ))
	then
		echo " !!! ERROR: Unknown measurement criteria [${criteria["$c"]}] specified via -$c parameter !!!" >&2
		exit 2
	fi
	criteria_index["$c"]=$i
done

############################################################################################################ Construct plot data:

check_number(){
	if [[ ! "$1" =~ ^[[:blank:]]*[-+]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?[[:blank:]]*$ ]]
	then
		echo " !!! ERROR: Measurement value [$1] is not a number !!!" >&2
		exit 2
	else
		printf "%s" "$( echo -e "$1" | tr -d '[:space:]' )"
	fi
	shift
}

value_axis="y"
if [ -v "criteria[z]" ]
then
	value_axis="z"
fi

declare -A existing_points
existing_points=()

for s in "${source_tables[@]}"
do
	while IFS='' read -r line
	do
		coordinate=""
		for c in "${!criteria[@]}"
		do
			start=$(( criteria_index["$c"] * 21 + criteria_index["$c"] + 1 ))
			end=$(( ( criteria_index["$c"] + 1 ) * 21 + criteria_index["$c"] ))
			cell_value="$( printf "%s" "$line" | cut -c $start-$end )"
			if [ ! "$c" = "l" ]
			then
				cell_value="$( check_number "$cell_value" )"
			fi
			if [ ! "$c" == "$value_axis" ]
			then
				coordinate="$coordinate | $c: $cell_value"
			fi
		done
		coordinate="${coordinate:2}"
		if [ -v "existing_points[$coordinate]" ]
		then
			echo " !!! ERROR: Duplicated measurement for [$coordinate] !!!" >&2
			exit 2
		else
			existing_points["$coordinate"]="true"
		fi
	done < <( tail -n +3 "$s" )
done

################################################################################# Finish execution & cleanup temporary resources:
exit 0 # triggers 'my_exit'
