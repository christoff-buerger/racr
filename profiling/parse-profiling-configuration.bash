#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# BEWARE: This script must be sourced. It expects one variables to be set and sets six variables:
#  in)  profiling_configuration:	Profiling configuration file to parse
#  set) parameter_names:		Array of the names of parameters as defined by the parsed configuration
#  set) parameter_descriptions:		Array of parameters descriptions as given by the parsed configuration
#					(one description for each parameter)
#  set) result_names:			Array of the names of measurement results as defined by the parsed configuration
#  set) result_descriptions:		Array of measurement result descriptions as given by the parsed configuration
#					(one description for each measurement result)
#  set) number_of_parameters:		Number of parameters defined by the parsed configuration
#  set) number_of_results:		Number of measurement results defined by the parsed configuration

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -z ${profiling_configuration+x} ] || [ ! -f "$profiling_configuration" ]
then
	echo " !!! ERROR: Non-existent or no profiling configuration to parse set !!!" >&2
	exit 2
fi

parsing_mode=parameters
parameter_names=()
parameter_descriptions=()
result_names=()
result_descriptions=()
while read line
do
	case $parsing_mode in
	parameters)
		if [ "$line" == ">" ]
		then
			parsing_mode=results
			continue;
		fi
		IFS='|' read -ra config_line <<< "$line"
		if [ ${#config_line[@]} -ne 2 ]
		then
			echo " !!! ERROR: Malformed profiling configuration (parameter syntax error) !!!" >&2
			exit 2
		fi
		parameter_names+=( "${config_line[0]}" )
		parameter_descriptions+=( "${config_line[1]}" );;
	results)
		IFS='|' read -ra config_line <<< "$line"
		if [ ${#config_line[@]} -ne 2 ]
		then
			echo " !!! ERROR: Malformed profiling configuration (measurement result syntax error) !!!" >&2
			exit 2
		fi
		result_names+=( "${config_line[0]}" )
		result_descriptions+=( "${config_line[1]}" );;
	esac
done < "$profiling_configuration"
number_of_parameters=${#parameter_names[@]}
number_of_results=${#result_names[@]}
