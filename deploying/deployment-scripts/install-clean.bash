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
			echo "          are cleaned for all available systems." >&2
			echo "" >&2
			echo "          Always all RACR library installations for a Scheme system are cleaned." >&2
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

################################# Lock all binaries to clean to prevent race-conditions with installations or executions of such:
locks=()

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release locks:
	for mutex in "${locks[@]}"
	do
		if [ -f "$mutex" ]
		then
			"$mutex"
		fi
	done
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

binaries_to_clean=()
for library in "${selected_libraries[@]}"
do
	for system in "${selected_systems[@]}"
	do
		if [ -d "$library/binaries/$system" ]
		then
			binaries_to_clean+=( "$library/binaries/$system" )
		fi
	done
done

mapfile -t locks < <(
	"$script_dir/lock-files.bash" \
	-x " !!! ERROR: Failed to clean installations; RACR libraries are in use or installations are in progress !!!" \
	-- "${binaries_to_clean[@]/%//lock}" \
	|| kill -13 $$ )

########################################################### Delete installation of all RACR libraries on selected Scheme systems:
for binaries in "${binaries_to_clean[@]}"
do
	rm -rf "${binaries:?}/"*
done

exit 0 # triggers 'my_exit'
