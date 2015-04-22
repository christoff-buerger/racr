#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################### Configure temporary resources & rerun script:
old_pwd=`pwd`

my_exit(){
	cd $old_pwd
	rm rerun-measurements.bash
	rm table-pipe
	exit 0
}
trap 'my_exit' 1 2 3 9 15

mkfifo table-pipe
./make-table.bash -c measurements.configuration -t table.txt -p table-pipe &
echo "cd ../.." > rerun-measurements.bash
echo "./make-measurements.bash << EOF" >> rerun-measurements.bash

################################################################################################################ Read parameters:
declare -a parameter_names
declare -a parameter_values
declare -a parameter_iterations
declare -a parameter_adjustments

echo "************************************************** Configure Parameters **************************************************"
exec 3< measurements.configuration
while read -r line <&3
do
	IFS='/' read -ra config_line <<< "$line"
	
	parameter_names+=( "${config_line[0]}" )
	
	read -r -p "${config_line[1]} [${config_line[0]}]: " choice
	if [ ! -t 0 ]; then echo "${config_line[1]} [${config_line[0]}]: $choice"; fi
	echo $choice >> $old_pwd/rerun-measurements.bash
	parameter_values+=( "$choice" )
	
	read -r -n1 -p "	Iterate? (y/n): " choice
	if [ ! -t 0 ]; then printf "	Iterate? (y/n): $choice"; fi
	echo ""
	printf $choice >> $old_pwd/rerun-measurements.bash
	case $choice in
		[y]* )	read -r -p "	Number of iterations: " choice
				if [ ! -t 0 ]; then echo "	Number of iterations: $choice"; fi
				echo $choice >> $old_pwd/rerun-measurements.bash
				parameter_iterations+=( "$choice" )
				read -r -p "	Adjustment each iteration: " choice
				if [ ! -t 0 ]; then echo "	Adjustment each iteration: $choice"; fi
				echo $choice >> $old_pwd/rerun-measurements.bash
				parameter_adjustments+=( "$choice" );;
		[n]* )	parameter_iterations+=( 1 )
				parameter_adjustments+=( 0 );;
		* ) echo "	!!! ERROR: No valid choice entered !!! "; my_exit;;
	esac
done
exec 3<&-

################################################################################### Create directories & finish the rerun script:
measurement_dir=$old_pwd/measurements/`date "+%Y-%m-%d_%H-%M-%S"`
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
exec 3> table-pipe
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
