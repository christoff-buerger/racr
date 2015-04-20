#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

# Function always called when the script terminates; cleanup temporary resources:
my_exit(){
	cd $old_pwd
	rm rerun-measurements.bash
	exit 0
}
trap 'my_exit' 1 2 3 9 15

# Persist configuration in a script such that the measurements can be rerun:
echo "cd ../.." > rerun-measurements.bash
echo "./make-measurements.bash << EOF" >> rerun-measurements.bash

# Read parameters:
declare -a parameter_names
declare -a parameter_values
while read line
do
	IFS='/' read -ra config_line <<< "$line"
	parameter_names+=( "${config_line[0]}" )
	read -u 3 -r -p "${config_line[1]}: " choice
	if [ ! -t 3 ]; then echo "${config_line[1]}: $choice"; fi
	echo $choice >> $old_pwd/rerun-measurements.bash
	parameter_values+=( "$choice" )
done 3<&0 < measurements.configuration

# Create directories:
measurement_dir=$old_pwd/measurements/`date "+%Y-%m-%d_%H-%M-%S"`
mkdir -p $measurement_dir

# Finish & copy the rerun script:
echo "EOF" >> rerun-measurements.bash
chmod +x rerun-measurements.bash
cp -p rerun-measurements.bash $measurement_dir

my_exit
