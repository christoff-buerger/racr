#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

selected_systems_array=()

################################################################################################################ Parse arguments:
while getopts s:h opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems_array+=( "$OPTARG" )
			;;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter)." >&2
			echo "          Permitted values:" >&2
			"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /' >&2
			echo "          If no Scheme system is selected, RACR library installations" >&2
			echo "          are cleared for all available systems." >&2
			exit 64
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

if [[ ! -v "selected_systems_array[@]" ]]
then
	mapfile -t selected_systems_array < <( "$script_dir/list-scheme-systems.bash" -i || kill -13 $$ )
fi
declare -A selected_systems # Use associative array as set, to avoid double entries.
selected_systems=()
for s in "${selected_systems_array[@]}"
do
	selected_systems["$s"]="$s"
done

mapfile -t selected_libraries < <( "$script_dir/list-libraries.bash" -i || kill -13 $$ )

########################################################### Delete installation of all RACR libraries on selected Scheme systems:
for library in "${selected_libraries[@]}"
do
	for system in "${selected_systems[@]}"
	do
		# Configure the directory for the Scheme system specific binaries:
		binaries="$library/binaries/$system"
		rm -rf "${binaries:?}/"*		
	done	
done
