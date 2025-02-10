#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

call_dir="$( pwd )"
unset failsave

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/measure.bash" -h
	exit $?
fi

while getopts c:t:p:s:xh opt
do
	case $opt in
		c)
			if [ "${profiling_configuration+x}" = "" ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi
			;;
		t)
			if [ "${measurements_table+x}" = "" ]
			then
				measurements_table="$OPTARG"
			else
				echo " !!! ERROR: Several measurements tables selected via -t parameter !!!" >&2
				exit 2
			fi
			;;
		p)
			if [[ ! -v "to_profile" ]]
			then
				to_profile="$OPTARG"
			else
				echo " !!! ERROR: Several programs to profile selected via -p parameter !!!" >&2
				exit 2
			fi
			;;
		s)
			if [ "${rerun_script+x}" = "" ]
			then
				rerun_script="$OPTARG"
			else
				echo " !!! ERROR: Several rerun script names selected via -s parameter !!!" >&2
				exit 2
			fi
			;;
		x)
			failsave="-x"
			;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -t Measurements table used for recording (mandatory parameter)." >&2
			echo "          Created if not existent. New measurements are appended." >&2
			echo "       -p Program profiled (mandatory parameter)." >&2
			echo "       -s Save rerun script (optional parameter)." >&2
			echo "          Can be used to redo the measurements." >&2
			echo "       -x Abort in case of measurement failures (optional multi-flag)." >&2
			echo "       -- Command line arguments for the profiled program (optional parameter)." >&2
			echo "          All following arguments are forwarded, after expected profiling parameters," >&2
			echo "          whenever calling the profiled program. The final order of call arguments is:" >&2
			echo "            1. profiling parameters ordered according to the profiling configuration" >&2
			echo "            2. arguments of the forwarded argument list (without leading --)" >&2
			echo "" >&2
			echo "       In case of any failed measurements, the exit status code returned" >&2
			echo "       after script termination is 1." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

fixed_arguments=( "$@" )

if [ -t 0 ] && [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ "$to_profile" = "" ] || [ ! -x "$to_profile" ]
then
	echo " !!! ERROR: No, non-existent or non-executable program to profile specified via -p parameter !!!" >&2
	exit 64
fi

if [ "${rerun_script+x}" = "" ]
then
	rerun_script="/dev/null"
elif [ "$rerun_script" = "" ] || { [ ! "$rerun_script" -ef "/dev/null" ] && [ -e "$rerun_script" ]; }
then
	echo " !!! ERROR: Invalid rerun script specified via -s parameter !!!" >&2
	exit 2
fi

##################################################################################### Configure temporary and external resources:
tmp_dir=""
recording_pid=""
valid_parameters=0

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Close the recording pipe and wait until all measurements are recorded:
	if [ "$recording_pid" != "" ]
	then
		exec 3>&-
		while s=$( ps -p "$recording_pid" -o state= ) && [[ "$s" && "$s" != 'Z' ]] 
		do
			sleep 1
		done
	fi
	# Delete the rerun script in case a user interactively specified invalid measurement-parameters while generating it:
	if [ -t 0 ] && [ "$exit_status" -gt 0 ] && [ "$valid_parameters" -eq 0 ] && [ ! "$rerun_script" -ef "/dev/null" ]
	then
		rm -f "$rerun_script"
	fi
	# Delete all temporary resources:
	rm -rf "$tmp_dir"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit "$exit_status"
}
trap 'my_exit' 0 1 2 3 15

tmp_dir="$( "$script_dir/../../deploying/deployment-scripts/create-temporary.bash" -t d )"
measurements_pipe="$tmp_dir/measurements-pipe.fifo"
measurements_stderr="$tmp_dir/measurements-errors.txt"

mkfifo "$measurements_pipe"
recording_pid=$( "$script_dir/record.bash" -c "$profiling_configuration" -t "$measurements_table" -p "$measurements_pipe" )
exec 3> "$measurements_pipe"

if [ ! "$rerun_script" -ef "/dev/null" ]
then
	mkdir -p "$( dirname "$rerun_script" )"
	touch "$rerun_script"
	chmod +x "$rerun_script"
fi
{
echo "#!/usr/bin/env bash"
echo "set -e"
echo "set -o pipefail"
echo "shopt -s inherit_errexit"
echo "if [ ! \$# -eq 0 ]"
echo "then"
echo "	echo \" !!! ERROR: Unknown [\$*] command line arguments !!!\" >&2"
echo "	exit 2"
echo "fi"
echo "cd \"$call_dir\""
echo "\"$script_dir/measure.bash\" \\"
echo "	-c \"$profiling_configuration\" \\"
echo "	-t \"$measurements_table\" \\"
echo "	-p \"$to_profile\" \\"
echo "	-s /dev/null \\"
if [[ -v "failsave" ]]
then
	echo "	-x \\"
fi
if (( ${#fixed_arguments[@]} != 0 ))
then
	printf "	--"
	for a in "${fixed_arguments[@]}"
	do
		printf " \"%s\"" "$a"
	done
	echo " \\"
fi
echo "	<< \"\|EOF\|\""
} > "$rerun_script"

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

################################################################################################################ Read parameters:
parameter_values=( "DUMMY DATE" )
parameter_iterations=( "DUMMY DATE ITERATION" )
parameter_adjustments=( "DUMMY DATE ADJUSTMENT" )

for (( i = 1; i < number_of_parameters; i++ ))
do
	IFS='' read -r -p "${parameter_descriptions[$i]} [${parameter_names[$i]}]: " choice
	if [ ! -t 0 ]
	then
		echo "${parameter_descriptions[$i]} [${parameter_names[$i]}]: $choice"
	fi
	echo "$choice" >> "$rerun_script"
	parameter_values+=( "$choice" )
	
	case "$choice" in
		''|*[0-9]*)
			IFS='' read -r -n1 -p "	Iterate? (y/n): " choice
			if [ ! -t 0 ]
			then
				printf "	Iterate? (y/n): %s" "$choice"
			fi
			echo ""
			printf "%s" "$choice" >> "$rerun_script"
			;;
		*)
			choice="n"
			;;
	esac
	case "$choice" in
		[y]*)
			IFS='' read -r -p "	Number of iterations (altogether): " choice
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
					fi
					;;
				*)
					echo " !!! ERROR: Invalid choice !!!" >&2
					exit 2
					;;
			esac
			parameter_iterations+=( "$choice" )
			IFS='' read -r -p "	Adjustment each iteration (excluding first): " choice
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
					exit 2
					;;
			esac
			parameter_adjustments+=( "$choice" )
			;;
		[n]*)
			parameter_iterations+=( 1 )
			parameter_adjustments+=( 0 )
			;;
		*)
			echo "	!!! ERROR: Invalid choice !!!" >&2
			exit 2
			;;
	esac
done
echo "\|EOF\|" >> "$rerun_script"
valid_parameters=1

########################################################################################################### Perform measurements:
echo ""

exit_status=0
current_parameter_values=( "DUMMY DATE" )
current_parameter_iterations=( "DUMMY DATE ITERATION" )
current_parameter=1
undo=false

while [ "$current_parameter" -ge 1 ]
do
	if (( current_parameter >= number_of_parameters ))
	then # perform measurement
		current_parameter=$(( current_parameter - 1 ))
		undo=true
		measurement_date="$( date -u "+%Y-%m-%dT%H:%M:%SZ" )"
		echo "$measurement_date" >&3
		printf "\033[0;34m"
		printf "Measurement ["
		for (( i = 1; i < number_of_parameters; i++ ))
		do
			printf " %s=%s " "${parameter_names[$i]}" "${current_parameter_values[$i]}"
			echo "${current_parameter_values[$i]}" >&3
		done
		echo "]"
		printf "\033[0m"
		parameter_arguments=( "${current_parameter_values[@]:1}" )
		old_IFS="$IFS"
		IFS=$'\n' # measurement results are emitted line-wise
		measurement_results=()
		mapfile -t measurement_results < <(
			set +e
			set +o pipefail
			"$to_profile" "${parameter_arguments[@]}" "${fixed_arguments[@]}" 2> "$measurements_stderr"
			echo $?
			set -e
			set -o pipefail
		)
		IFS="$old_IFS"
		measurement_error="${measurement_results[-1]}"
		unset 'measurement_results[-1]'
		
		measurement_status="succeeded"
		print_color="\033[0;32m"
		print_stderr_message="false"
		print_results="false"
		if [ -s "$measurements_stderr" ]
		then
			print_color="\033[1;33m"
			print_stderr_message="true"
		fi
		if (( measurement_error != 0 ))
		then
			measurement_status="failed"
			print_color="\033[0;31m"
			print_stderr_message="true"
		elif (( ${#measurement_results[@]} + 1 != number_of_results ))
		then
			measurement_status="failed"
			print_color="\033[0;31m"
			echo "	!!! ERROR: Unexpected number of measurement results !!!" >&2
		else
			print_results="true"
		fi
		
		echo "$measurement_status" >&3 # 1st result is the measurement status.
		
		if [[ "$print_stderr_message" == "true" ]]
		then
			printf "%b" "$print_color" >&2
			printf "  Measurement %s with exit code [%i] and " "$measurement_status" "$measurement_error" >&2
			if [ -s "$measurements_stderr" ]
			then
				echo "error message (stderr):" >&2
				# Command substitution removes trailing newlines from compacted message prefixed with ' >>':
				printf "%s\n" "$( sed '/^[[:space:]]*$/d' "$measurements_stderr" | sed -e 's/^/  >>/' )" >&2
			else
				echo "no error message (empty stderr)." >&2
			fi
			printf "\033[0m" >&2
		fi
		if [[ "$print_results" == "true" ]]
		then
			printf "%b" "$print_color"
			for (( i = 1; i < number_of_results; i++ ))
			do
				echo "  ${result_names[$i]}=${measurement_results[$(( i - 1 ))]}"
				echo "${measurement_results[$(( i - 1 ))]}" >&3
			done
			printf "\033[0m"
		fi
		
		if [[ "$measurement_status" == "failed" ]]
		then
			exit_status=1
			if [[ -v "failsave" ]]
			then
				echo " !!! ERROR: Measurements aborted because of failed measurement !!!" >&2
				exit 2
			elif [[ "$print_results" != "true" ]] # fix table
			then
				for (( i = 1; i < number_of_results; i++ ))
				do
					echo "" >&3
				done
			fi
		fi
	elif [ "$undo" = true ] && \
		(( current_parameter_iterations[current_parameter] < parameter_iterations[current_parameter] ))
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

################################################################# Finish recording of measurements & cleanup temporary resources:
exit "$exit_status" # triggers 'my_exit'
