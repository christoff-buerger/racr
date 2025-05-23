#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if (( $# != 0 ))
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

############################################################################################################ Configure resources:
mutex=""

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release lock:
	if [[ -f "$mutex" ]]
	then
		"$mutex"
	fi
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit "$exit_status"
}
trap 'my_exit' 0 1 2 3 15

mutex="$( "$script_dir/../deploying/deployment-scripts/lock-files.bash" \
	-x " !!! ERROR: RACR-NET tests already in execution !!!" \
	-- "$script_dir/execute.bash" )"

if [[ ! -e "$script_dir/nunit" ]]
then
	mkdir -p "$script_dir/nunit"
	(
		cd "$script_dir/nunit"
		NuGet install NUnit
		NuGet install NUnit.Console
	)
fi

################################################################################################################## Execute tests:
(
	cd "$script_dir/binaries"
	mono "$script_dir/nunit/NUnit.ConsoleRunner."*.*.*"/tools/nunit3-console.exe" "$script_dir/binaries/Test.dll"
)
