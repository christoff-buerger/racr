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
	"$script_dir/make-measurements.bash" -h
	exit $?
fi
while getopts c:s:h opt
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
		s)
			if [ -z ${rerun_script+x} ]
			then
				rerun_script="$OPTARG"
			else
				echo " !!! ERROR: Several rerun script names selected via -s flag !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -s Save rerun script (optional parameter)." >&2
			echo "          Can be used to redo the measurements." >&2
			echo "          Generated in the 'measurements' directory of the used configuration." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
if [ -t 0 ] && [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${profiling_configuration+x} ] || [ ! -f "$profiling_configuration" ]
then
	echo " !!! ERROR: Non-existing or no profiling configuration specified via -c flag !!!" >&2
	exit 2
fi

measurements_date=`date "+%Y-%m-%d_%H-%M-%S"`
measurements_pipe="$script_dir/$measurements_date.measurements-pipe"
measurements_dir="$script_dir/`dirname "$profiling_configuration"`/measurements"
measurements_table="$measurements_dir/measurements-table.txt"
valid_parameters=0

if [ -z ${rerun_script+x} ]
then
	rerun_script="/dev/null"
else
	rerun_script_basename="`basename "$rerun_script"`"
	if [ "$rerun_script_basename" != "$rerun_script" ]
	then
		echo " !!! ERROR: Invalid name for rerun script specified via -s flag !!!" >&2
		exit 2
	else
		rerun_script="$measurements_dir/$rerun_script"
	fi
	if [ -e "$rerun_script" ]
	then
		echo " !!! ERROR: Rerun script specified via -s flag already exists !!!" >&2
		exit 2
	fi
fi

##################################################################################### Configure temporary and external resources:

my_exit(){
	exit_status=$?
	if [ $exit_status -gt 0 ] && [ $valid_parameters -eq 0 ] && [ -t 0 ] && [ "$rerun_script" != "/dev/null" ]
	then
		rm "$rerun_script"
	fi
	rm -f "$measurements_pipe"
	exit $exit_status
}
trap 'my_exit' 1 2 3 9 15

mkfifo "$measurements_pipe"
"$script_dir/make-table.bash" -c "$profiling_configuration" -t "$measurements_table" -p "$measurements_pipe" &



sleep 1
echo " !!! ABORT: Not implemented yet !!!"
my_exit



################################################################################################################ Read parameters:
declare -a parameter_names
declare -a parameter_values
declare -a parameter_iterations
declare -a parameter_adjustments

echo "************************************************** Configure Parameters **************************************************"
echo "$script_dir/make-measurements.bash << EOF" > "$rerun_script"
exec 3< measurements.configuration
while read -r line <&3
do
	IFS='/' read -ra config_line <<< "$line"
	
	parameter_names+=( "${config_line[0]}" )
	
	read -r -p "${config_line[1]} [${config_line[0]}]: " choice
	if [ ! -t 0 ]; then echo "${config_line[1]} [${config_line[0]}]: $choice"; fi
	echo $choice >> $script_dir/rerun-measurements.bash
	parameter_values+=( "$choice" )
	
	read -r -n1 -p "	Iterate? (y/n): " choice
	if [ ! -t 0 ]; then printf "	Iterate? (y/n): $choice"; fi
	echo ""
	printf $choice >> $script_dir/rerun-measurements.bash
	case $choice in
		[y]* )	read -r -p "	Number of iterations: " choice
				if [ ! -t 0 ]; then echo "	Number of iterations: $choice"; fi
				echo $choice >> $script_dir/rerun-measurements.bash
				parameter_iterations+=( "$choice" )
				read -r -p "	Adjustment each iteration: " choice
				if [ ! -t 0 ]; then echo "	Adjustment each iteration: $choice"; fi
				echo $choice >> $script_dir/rerun-measurements.bash
				parameter_adjustments+=( "$choice" );;
		[n]* )	parameter_iterations+=( 1 )
				parameter_adjustments+=( 0 );;
		* ) echo "	!!! ERROR: No valid choice entered !!! "; my_exit;;
	esac
done
exec 3<&-
valid_parameters=1

################################################################################### Create directories & finish the rerun script:
measurement_dir=$script_dir/measurements/`date "+%Y-%m-%d_%H-%M-%S"`
mkdir -p $measurement_dir

echo "EOF" >> rerun-measurements.bash
chmod +x rerun-measurements.bash
cp -p rerun-measurements.bash $measurement_dir

########################################################################################################### Perform measurements:
num_parameters=${#parameter_names[@]}
current_parameter=0
current_values[0]=$(( parameter_values[0] - parameter_adjustments[0] ))
run=true
undo=false

echo "*************************************************** Start Measurements ***************************************************"
exec 3> "$table_pipe"
while [ "$run" = true ]
do
	if (( current_parameter >= num_parameters ))
	then
		printf "Measurement ["
		undo=true
		run=false
		for (( i = 0; i < num_parameters; i++ ))
		do
			printf " ${parameter_names[$i]}=${current_values[$i]} "
			echo "${current_values[$i]}" >&3
			if (( current_iterations[i] < parameter_iterations[i] ))
			then
				run=true
			fi
		done
		echo "]"
		echo "12445" >&3
	fi
	if [ "$undo" = true ]
	then
		current_parameter=$current_parameter-1
		if (( current_iterations[current_parameter] < parameter_iterations[current_parameter] ))
		then
			undo=false
		fi
	else
		current_values[$current_parameter]=$(( current_values[current_parameter] + parameter_adjustments[current_parameter] ))
		current_iterations[$current_parameter]=$(( current_iterations[current_parameter] + 1 ))
		current_parameter=$(( current_parameter + 1 ))
		current_values[$current_parameter]=$(( parameter_values[current_parameter] - parameter_adjustments[current_parameter]))
		current_iterations[$current_parameter]=0
	fi
done
exec 3>&-
echo "**************************************************** End Measurements ****************************************************"

################################################################################# Finish execution & cleanup temporary resources:
my_exit
