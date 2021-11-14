#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

tmp_dir="$script_dir/temporary-files"

################################################################################################################ Parse arguments:
if [ $# -eq 0 ]
then
	"$script_dir/create-temporary.bash" -h
	exit $?
fi

while getopts t:n:u:h opt
do
	case $opt in
		t)
			if [ -z ${temporary_type+x} ]
			then
				temporary_type="$OPTARG"
			else
				echo " !!! ERROR: Several temporary-types selected via -t parameter !!!" >&2
				exit 2
			fi
			;;
		n)
			if [ -z ${name+x} ]
			then
				name="$OPTARG"
			else
				echo " !!! ERROR: Several names selected via -n parameter !!!" >&2
				exit 2
			fi
			;;
		u)
			if [ -z ${use_existing+x} ]
			then
				use_existing="$OPTARG"
			else
				echo " !!! ERROR: Several temporary directories selected via -u parameter !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -t Type of temporary to create (mandatory parameter)." >&2
			echo "          Must be one of the following: f (file), d (directory), p (pipe)." >&2
			echo "       -n Name of temporary to create (optional parameter)." >&2
			echo "          Forces the created temporary to have a certain basename." >&2
			echo "       -u Use existing temporary directory (optional parameter)." >&2
			echo "          Forces the created temporary to be within an existing temporary directory." >&2
			echo "          If also -n is set, and a respective temporary already exists in the used" >&2
			echo "          directory, the created temporary will be in a newly created direct" >&2
			echo "          subdirectory of the used one." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

case "$temporary_type" in
	f|file)
		temporary_type="file"
		;;
	d|directory)
		temporary_type="directory"
		;;
	p|pipe)
		temporary_type="pipe"
		;;
	*)
		temporary_type=""
		;;
esac
if [ -z "$temporary_type" ]
then
	echo " !!! ERROR: No or unknown temporary-type selected via -t parameter !!!" >&2
	exit 2
fi

if [ -n "${use_existing+x}" ]
then
	if [ ! -d "$use_existing" ]
	then
		echo " !!! ERROR: Non-existent directory selected via -u parameter !!!" >&2
		exit 2
	fi
	use_existing="$( cd "$use_existing" && pwd )"
	if [ "${use_existing##"$tmp_dir"}" == "$use_existing" ]
	then
		echo " !!! ERROR: No temporary directory selected via -u parameter !!!" >&2
		exit 2
	fi
	tmp_dir="$use_existing"
fi

############################################################################################################ Configure resources:
my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release lock:
	"$mutex"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}

mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/create-temporary.bash" )"
trap 'my_exit' 0 1 2 3 15

############################################################################################################### Create temporary:
mkdir -p "$tmp_dir"
tmp_name="$name"
while [ -e "$tmp_dir/$tmp_name" ]
do
	current_date="$( date -u "+%Y-%m-%dT%H:%M:%SZ" )" # Date in xs:dateTime format.
	tmp_name="$current_date-$(( RANDOM % 10 ))$(( RANDOM % 10 ))$(( RANDOM % 10 ))$(( RANDOM % 10 ))"
	if [ -n "${name+x}" ]
	then
		tmp_name="$tmp_name/$name"
	fi
done
mkdir -p "$( dirname "$tmp_dir/$tmp_name" )"
set +e
set +o pipefail
case $temporary_type in
	file)
		touch "$tmp_dir/$tmp_name" 2>/dev/null
		;;
	directory)
		mkdir -p "$tmp_dir/$tmp_name" 2>/dev/null
		;;
	pipe)
		mkfifo "$tmp_dir/$tmp_name" 2>/dev/null
		;;
esac
invalid_name=$?
set -e
set -o pipefail
if [ $invalid_name -ne 0 ]
then
	echo " !!! ERROR: [$name] is an invalid $temporary_type name !!!" >&2
	exit 2
fi
echo "$tmp_dir/$tmp_name"

############################################################################################################## Cleanup resources:
exit 0 # triggers 'my_exit'
