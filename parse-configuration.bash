#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# BEWARE: This script must be sourced. It expects one variables to be set and sets four variables:
#  in)  configuration_to_parse:		Configuration file to parse
#  set) configuration_directory:	Directory containing the configuration file to parse
#  set) supported_systems:		Array of KNOWN Scheme systems supported according to the parsed configuration
#  set) required_libraries:		Array of paths to the libraries required according to the parsed configuration
#  set) required_sources:		Array of paths to the source files according to the parsed configuration

parse_script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -z ${configuration_to_parse+x} ] || [ ! -f "$configuration_to_parse" ]
then
	echo " !!! ERROR: Non-existent or no configuration to parse set !!!" >&2
	exit 2
fi

parsing_mode=initial
configuration_directory=`dirname "$configuration_to_parse"`
supported_systems=()
required_libraries=( "$configuration_directory" )
required_sources=()
while read line
do
	case $parsing_mode in
	initial)
		if [ "$line" = "@systems:" ]
		then
			parsing_mode=systems
			continue
		fi
		supported_systems=( `"$parse_script_dir/list-scheme-systems.bash" -k` )
		if [ "$line" = "@libraries:" ]
		then
			parsing_mode=libraries
			continue
		fi
		if [ "$line" = "@sources:" ]
		then
			parsing_mode=sources
			continue
		fi
		parsing_mode=sources
		required_sources+=( "$configuration_directory/$line" );;
	systems)
		if [ "$line" = "@libraries:" ]
		then
			parsing_mode=libraries
			continue
		fi
		if [ "$line" = "@sources:" ]
		then
			parsing_mode=sources
			continue
		fi
		supported_systems+=( "$line" )
		continue;;
	libraries)
		if [ "$line" = "@sources:" ]
		then
			parsing_mode=sources
			continue
		fi
		required_libraries+=( "$configuration_directory/$line" );;
	sources)
		required_sources+=( "$configuration_directory/$line" );;
	esac
done < "$configuration_to_parse"
