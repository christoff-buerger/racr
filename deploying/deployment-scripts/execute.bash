#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

configurations_array=()

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if (( $# == 0 ))
then
	"$script_dir/execute.bash" -h
	exit $?
fi

while getopts s:e:l:h opt
do
	case $opt in
		s)
			if [[ ! -v "selected_system" ]]
			then
				"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
				selected_system="$OPTARG"
			else
				echo " !!! ERROR: Several Scheme systems for execution selected via -s parameter !!!" >&2
				exit 64
			fi
			;;
		e)
			if [[ ! -v "to_execute" ]]
			then
				to_execute="$OPTARG"
				to_execute_dir="$( dirname "$to_execute" )"
				if [[ "$( "$script_dir/list-libraries.bash" -c "$to_execute_dir" 2> /dev/null )" != "" ]]
				then
					configurations_array+=( "$( "$script_dir/list-libraries.bash" -c "$to_execute_dir" )" )
				fi
			else
				echo " !!! ERROR: Several programs to execute specified via -e parameter !!!" >&2
				exit 64
			fi
			;;
		l)
			configurations_array+=( "$( "$script_dir/list-libraries.bash" -c "$OPTARG" )" )
			;;
		h|*)
			echo "Usage: -s Scheme system (mandatory parameter). Permitted values:" >&2
			"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /' >&2
			echo "       -e Scheme program to execute (mandatory parameter)." >&2
			echo "       -l Load a RACR library before execution (optional parameter)." >&2
			echo "          The given argument must be a RACR library directory." >&2
			echo "          Know RACR library directories are:" >&2
			"$script_dir/list-libraries.bash" -i | sed 's/^/             /' >&2
			echo "          Implicitly set if the program to execute already is in a RACR library directory." >&2
			echo "       -- Command line arguments for the Scheme program to execute (optional parameter)." >&2
			echo "          All following arguments are forwarded to the executed program." >&2
			exit 64
			;;
	esac
done
shift $(( OPTIND - 1 ))

if (( $# != 0 )) && [[ " $* --" != "$arguments" ]]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

if [[ "$to_execute" == "" ]] || [[ ! -f "$to_execute" ]]
then
	echo " !!! ERROR: Non-existent or no Scheme program to execute specified via -e parameter !!!" >&2
	exit 64
fi

if [[ "$selected_system" == "" ]]
then
	echo " !!! ERROR: No Scheme system for execution selected via -s parameter !!!" >&2
	exit 64
fi

required_libraries=()
if (( ${#configurations_array[@]} > 1 ))
then
	echo " !!! ERROR: Several libraries to use specified, either via -l parameter or implicitly" >&2
	echo "            because the program to execute is in a RACR library directory !!!" >&2
	exit 64
elif (( ${#configurations_array[@]} == 1 ))
then
	configuration_to_parse="${configurations_array[0]}"
	. "$script_dir/configure.bash" # Sourced script sets configuration!
	required_libraries+=( "$configuration_directory" )
	if [[ ! -v "supported_systems[$selected_system]" ]]
	then
		echo " !!! ERROR: Scheme system [$selected_system] not supported by the program !!!" >&2
		exit 64
	fi	
else
	required_libraries+=( "$script_dir/../../racr" )
	required_libraries+=( "$script_dir/../../racr-meta" )
fi

###################################### Lock binaries of all used libraries to prevent race-conditions with installations of such:
locks=()

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release locks:
	for mutex in "${locks[@]}"
	do
		if [[ -f "$mutex" ]]
		then
			"$mutex"
		fi
	done
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit "$exit_status"
}
trap 'my_exit' 0 1 2 3 15

required_binary_locks=()
for l in "${required_libraries[@]}"
do
	required_binary_locks+=( "$l/binaries/$selected_system/lock" )
done

mapfile -t locks < <(
	"$script_dir/lock-files.bash" \
	-k "$script_dir/execute.bash" \
	-x " !!! ERROR: Can not execute [$to_execute]; installation of RACR libraries for $selected_system in progress !!!" \
	-- "${required_binary_locks[@]}" \
	|| kill -13 $$ )

################################################################################################################ Execute program:
case $selected_system in
	racket)
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs+=( ++path "$l/binaries/racket" )
		done
		plt-r6rs "${libs[@]}" "$to_execute" "$@"
		;;
	guile)
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs+=( -L "$l/binaries/guile" )
			libs+=( -C "$l/binaries/guile" )
		done
		guile --no-auto-compile "${libs[@]}" -s "$to_execute" "$@"
		;;
	larceny)
		libs_string=""
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs_string+=":$l/binaries/larceny"
		done
		if [[ "$libs_string" != "" ]]
		then
			libs+=( --path "${libs_string:1}" )
		fi
		larceny --utf8 --r6rs "${libs[@]}" --program "$to_execute" -- "$@"
		;;
	chez)
		libs_string=""
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs_string+=":$l/binaries/chez"
		done
		if [[ "$libs_string" != "" ]]
		then
			libs+=( --libdirs "${libs_string:1}" )
		fi
		"$( command -v chez || command -v chezscheme || command -v chez-scheme || command -v scheme )" \
			"${libs[@]}" --program "$to_execute" "$@"
		;;
	sagittarius)
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs+=( --loadpath="$l/binaries/sagittarius" )
		done
		sagittarius "${libs[@]}" "$to_execute" "$@"
		;;
	ypsilon)
		libs_string="$( dirname "$( command -v ypsilon )")/sitelib"
		for l in "${required_libraries[@]}"
		do
			libs_string+=":$l/binaries/ypsilon"
		done
		sitelib="--sitelib=$libs_string"
		loadpath="--loadpath=$libs_string"
		cache="$script_dir/../../racr/binaries/ypsilon"
		mkdir -p "$cache"
		# To reconstruct compiled-cache add --clean-acc flag.
		# To add compilation warnings add --warning flag.
		# To echo load and compile activities add --verbose flag.
		# The heap limit is in MBytes.
		ypsilon "$sitelib" "$loadpath" --acc="$cache" --quiet --r6rs --heap-limit=512 -- "$to_execute" "$@"
		;;
	ironscheme)
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs+=( -I "$l/binaries/ironscheme" )
		done
		mono "$( command -v IronScheme.Console-v4.exe )" -nologo "${libs[@]}" "$to_execute" "$@"
		;;
	*)
		echo " !!! INTERNAL SCRIPT ERROR: Unexpected [$selected_system] value for \$selected_system !!!" >&2
		exit 2
		;;
esac

exit 0 # triggers 'my_exit'
