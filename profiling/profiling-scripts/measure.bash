#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
call_dir=`pwd`

################################################################################################################ Parse arguments:
if [ $# -eq 0 ]
then
	"$script_dir/measure.bash" -h
	exit $?
fi
while getopts c:s:xh opt
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
		x)
			failsave="-x";;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -s Save rerun script (optional parameter)." >&2
			echo "          Can be used to redo the measurements." >&2
			echo "          Generated in the 'measurements' directory of the used profiling configuration." >&2
			echo "       -x Abort in case of measurement failures (optional multi-flag)." >&2
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
measurements_dir="`dirname "$profiling_configuration"`/measurements"
measurements_table="$measurements_dir/measurements-table.txt"
measurement_stderr="$script_dir/$measurements_date.stderr"
valid_parameters=0

if [ -z ${rerun_script+x} ]
then
	rerun_script="/dev/null"
elif [ ! "$rerun_script" -ef "/dev/null" ]
then
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

if [ -z ${failsave+x} ]
then
	failsave=""
fi

##################################################################################### Configure temporary and external resources:
my_exit(){
	exit_status=$?
	if [ -t 0 ] && [ $exit_status -gt 0 ] && [ $valid_parameters -eq 0 ] && [ ! "$rerun_script" -ef "/dev/null" ]
	then # user specified invalid measurement-parameters while generating rerun script
		rm "$rerun_script"
	fi
	rm -f "$measurements_pipe"
	rm -f "$measurement_stderr"
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 9 15

mkfifo "$measurements_pipe"
if [ ! -e "$measurements_dir" ]
then
	mkdir -p "$measurements_dir"
fi
"$script_dir/record.bash" -c "$profiling_configuration" -t "$measurements_table" -p "$measurements_pipe" &
if [ ! "$rerun_script" -ef "/dev/null" ]
then
	touch "$rerun_script"
	chmod +x "$rerun_script"
fi

echo "#!/bin/bash" >> "$rerun_script"
echo "set -e" >> "$rerun_script"
echo "set -o pipefail" >> "$rerun_script"
echo "cd \"$call_dir\"" >> "$rerun_script"
echo "\"$script_dir/measure.bash\" $failsave -c \"$profiling_configuration\" -s /dev/null -- << EOF" >> "$rerun_script"

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

################################################################################################################ Read parameters:
declare -a parameter_values
declare -a parameter_iterations
declare -a parameter_adjustments

for (( i = 0; i < number_of_parameters; i++ ))
do
	read -r -p "${parameter_descriptions[$i]} [${parameter_names[$i]}]: " choice
	if [ ! -t 0 ]
	then
		echo "${parameter_descriptions[$i]} [${parameter_names[$i]}]: $choice"
	fi
	echo "$choice" >> "$rerun_script"
	parameter_values+=( "$choice" )
	
	case "$choice" in
		''|*[0-9]*)
			read -r -n1 -p "	Iterate? (y/n): " choice
			if [ ! -t 0 ]
				then printf "	Iterate? (y/n): $choice"
			fi
			echo ""
			printf "$choice" >> "$rerun_script";;
		*)
			choice="n";;
	esac
	case "$choice" in
		[y]*)
			read -r -p "	Number of iterations (altogether): " choice
			if [ ! -t 0 ]
			then
				echo "	Number of iterations (altogether): $choice"
			fi
			echo "$choice" >> "$rerun_script"
			case "$choice" in
				''|*[0-9]*)
					if [ "$choice" -lt 1 ]
					then
						echo " !!! ERROR: Invalid choice !!!" >&2
						exit 2
					fi;;
				*)
					echo " !!! ERROR: Invalid choice !!!" >&2
					exit 2;;
			esac
			parameter_iterations+=( "$choice" )
			read -r -p "	Adjustment each iteration (excluding first): " choice
			if [ ! -t 0 ]
			then
				echo "	Adjustment each iteration (excluding first): $choice"
			fi
			echo "$choice" >> "$rerun_script"
			case "$choice" in
				''|*[0-9]*)
					;;
				*)
					echo " !!! ERROR: Invalid choice !!!" >&2
					exit 2;;
			esac
			parameter_adjustments+=( "$choice" );;
		[n]*)
			parameter_iterations+=( 1 )
			parameter_adjustments+=( 0 );;
		*)
			echo "	!!! ERROR: Invalid choice !!!" >&2
			exit 2;;
	esac
done
echo "EOF" >> "$rerun_script"
valid_parameters=1

########################################################################################################### Perform measurements:
echo ""

current_parameter=0
undo=false
declare -a current_parameter_values
declare -a current_parameter_iterations

exec 3> "$measurements_pipe"
while [ $current_parameter -ge 0 ]
do
	if (( current_parameter >= number_of_parameters ))
	then # perform measurement
		current_parameter=$(( current_parameter - 1 ))
		undo=true
		printf "Measurement ["
		for (( i = 0; i < number_of_parameters; i++ ))
		do
			printf " ${parameter_names[$i]}=${current_parameter_values[$i]} "
			echo "${current_parameter_values[$i]}" >&3
		done
		echo "]"
		measurement_date=`date "+%Y-%m-%d %H:%M:%S"`
		echo "$measurement_date" >&3
		old_IFS="$IFS"
		IFS=$'\n'
		set +e
		set +o pipefail
		measurement_results=( `"$execution_script" ${current_parameter_values[@]} 2> "$measurement_stderr"` )
		measurement_error=$?
		set -e
		set -o pipefail
		IFS="$old_IFS"
		if [ $measurement_error -ne 0 -o -s "$measurement_stderr" ]
		then
			measurement_failed=1
		elif [ ${#measurement_results[@]} -ne $number_of_results ]
		then
			echo "	!!! ERROR: Unexpected number of measurement results !!!" >&2
			measurement_failed=1
		else
			measurement_failed=0
		fi
		if [ $measurement_failed -ne 0 ]
		then
			echo "X" >&3
			echo "	Measurement failed."
			if [ $measurement_error -ne 0 ]
			then
				echo "	The error code was: $measurement_error"
			fi
			if [ -s "$measurement_stderr" ]
			then
				echo "	The error message was:"
				cat "$measurement_stderr" >&2
				echo ""
			fi
			if [ ! -z "$failsave" ]
			then
				echo " !!! ERROR: Measurements aborted because of failed measurement !!!" >&2
				exit 2
			fi
		else
			echo "-" >&3
			for (( i = 0; i < number_of_results; i++ ))
			do
				echo "	${result_names[$i]}=${measurement_results[$i]}"
				echo "${measurement_results[$i]}" >&3
			done
		fi
	elif [ "$undo" = true ] && (( current_parameter_iterations[current_parameter] < parameter_iterations[current_parameter] ))
	then # redo with adjusted parameters
		undo=false
		current_parameter_values[$current_parameter]=$((
			current_parameter_values[current_parameter] + parameter_adjustments[current_parameter] ))
		current_parameter_iterations[$current_parameter]=$(( current_parameter_iterations[current_parameter] + 1 ))
		current_parameter=$(( current_parameter + 1 ))
	elif [ "$undo" = true ]
	then # reinitialise and further backtrack
		current_parameter_values[$current_parameter]=${parameter_values[$current_parameter]}
		current_parameter_iterations[$current_parameter]=1
		current_parameter=$(( current_parameter - 1 ))
	else # initialise
		current_parameter_values[$current_parameter]=${parameter_values[$current_parameter]}
		current_parameter_iterations[$current_parameter]=1
		current_parameter=$(( current_parameter + 1 ))
	fi
done
exec 3>&-

################################################################################# Finish execution & cleanup temporary resources:
my_exit
