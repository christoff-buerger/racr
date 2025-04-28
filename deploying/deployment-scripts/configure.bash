#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# BEWARE: This script must be sourced. It expects one variables to be set and sets four variables:
#  in)  configuration_to_parse:	 Configuration file to parse
#  set) configuration_directory: Directory containing the configuration file to parse
#  set) supported_systems:	 Associative array of KNOWN Scheme systems supported according to the parsed configuration
#  set) required_libraries:	 Array of paths to the libraries required according to the parsed configuration
#  set) required_sources:	 Array of paths to the source files according to the parsed configuration

set -e
set -o pipefail
shopt -s inherit_errexit
configure_bash_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ ! -v "configuration_to_parse" ]] || [[ ! -f "$configuration_to_parse" ]]
then
	echo " !!! ERROR: Non-existent or no configuration to parse set !!!" >&2
	exit 64
fi

parsing_mode=initial
configuration_directory="$( cd "$( dirname "$configuration_to_parse" )" && pwd )"
supported_systems_array=()
required_libraries=()
required_sources=()

while IFS='' read -r line
do
	case $parsing_mode in
	initial)
		if [[ "$line" == "@systems:" ]]
		then
			parsing_mode=systems
			continue
		fi
		mapfile -t supported_systems_array < <( "$configure_bash_dir/list-scheme-systems.bash" -k || kill -13 $$ )
		if [[ "$line" == "@libraries:" ]]
		then
			parsing_mode=libraries
			continue
		fi
		if [[ "$line" == "@sources:" ]]
		then
			parsing_mode=sources
			continue
		fi
		parsing_mode=sources
		required_sources+=( "$configuration_directory/$line" )
		;;
	systems)
		if [[ "$line" == "@libraries:" ]]
		then
			parsing_mode=libraries
			continue
		fi
		if [[ "$line" == "@sources:" ]]
		then
			parsing_mode=sources
			continue
		fi
		supported_systems_array+=( "$line" )
		continue
		;;
	libraries)
		if [[ "$line" == "@sources:" ]]
		then
			parsing_mode=sources
			continue
		fi
		required_libraries+=( "$( cd "$configuration_directory/$line" && pwd )" )
		;;
	sources)
		required_sources+=( "$configuration_directory/$line" )
		;;
	*)
		echo " !!! ERROR: Failed to process RACR library configuration [$configuration_to_parse] !!!" >&2
		exit 2
		;;
	esac
done < "$configuration_to_parse"

declare -A supported_systems
supported_systems=() # Set separately from declaration since script may be sourced several times for varying configurations!
for s in "${supported_systems_array[@]}"
do
	# shellcheck disable=SC2034
	supported_systems["$s"]="$s"
done

unset supported_systems_array
unset parsing_mode
unset configure_bash_dir
