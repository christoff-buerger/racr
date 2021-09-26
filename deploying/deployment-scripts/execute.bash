#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/execute.bash" -h
	exit $?
fi

while getopts s:e:l:h opt
do
	case $opt in
		s)
			if [ -z ${selected_system+x} ]
			then
				"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
				selected_system="$OPTARG"
			else
				echo " !!! ERROR: Several Scheme systems for execution selected via -s parameter !!!" >&2
				exit 2
			fi
			;;
		e)
			if [ -z ${to_execute+x} ]
			then
				to_execute="$OPTARG"
				to_execute_dir="$( dirname "$to_execute" )"
				if [ -n "$( "$script_dir/list-libraries.bash" -c "$to_execute_dir" 2> /dev/null )" ]
				then
					configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$to_execute_dir" )"
				fi
			else
				echo " !!! ERROR: Several programs to execute specified via -e parameter !!!" >&2
				exit 2
			fi
			;;
		l)
			if [ -z ${configuration_to_parse+x} ]
			then
				configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$OPTARG" )"
			else
				echo " !!! ERROR: Several libraries to use specified, either via -l parameter or implicitly" >&2
				echo "            because the program to execute is in a RACR library directory !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -s Scheme system (mandatory parameter). Permitted values:" >&2
			"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /' >&2
			echo "       -e Scheme program to execute (mandatory parameter)." >&2
			echo "       -l Load a RACR library before execution (optional parameter)." >&2
			echo "          The given argument must be a RACR library directory." >&2
			echo "          Know RACR library directories are:" >&2
			"$script_dir/list-libraries.bash" -i | sed 's/^/             /' >&2
			echo "          Implicitly set if the program to execute already is in a RACR library directory." >&2
			echo "       -- Command line arguments for the Scheme program to execute (optional parameter). " >&2
			echo "          All following arguments are forwarded to the executed program." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z "$to_execute" ] || [ ! -f "$to_execute" ]
then
	echo " !!! ERROR: Non-existent or no Scheme program to execute specified via -e parameter !!!" >&2
	exit 2
fi

if [ -z "$selected_system" ]
then
	echo " !!! ERROR: No Scheme system for execution selected via -s parameter !!!" >&2
	exit 2
fi

if [ -z ${configuration_to_parse+x} ]
then
	required_libraries=( "$script_dir/../../racr" )
	required_libraries+=( "$script_dir/../../racr-meta" )
else
	. "$script_dir/configure.bash" # Sourced script sets configuration!
	if [ ! ${supported_systems["$selected_system"]+x} ]
	then
		echo " !!! ERROR: Scheme system [$selected_system] not supported by the program !!!" >&2
		exit 2
	fi
fi

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
		if [ -n "$libs_string" ]
		then
			libs+=( --path "${libs_string:1}" )
		fi
		larceny --r6rs "${libs[@]}" --program "$to_execute" -- "$@"
		;;
	chez)
		libs_string=""
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs_string+=":$l/binaries/chez"
		done
		if [ -n "$libs_string" ]
		then
			libs+=( --libdirs "${libs_string:1}" )
		fi
		chez "${libs[@]}" --program "$to_execute" "$@"
		;;
	sagittarius)
		libs=()
		for l in "${required_libraries[@]}"
		do
			libs+=( --loadpath="$l/.." )
		done
		sagittarius "${libs[@]}" "$to_execute" "$@"
		;;
	ypsilon)
		libs_string=""
		for l in "${required_libraries[@]}"
		do
			libs_string+=":$l/.."
		done
		if [ -n "$libs_string" ]
		then
			libs_string="${libs_string:1}"
			sitelib="--sitelib=$libs_string"
			loadpath="--loadpath=$libs_string"
		fi
		cache="$script_dir/../../racr/binaries/ypsilon"
		mkdir -p "$cache"
		# To reconstruct compiled-cache add --clean-acc flag.
		# To add compilation warnings add --warning flag.
		# To echo load and compile actions add --verbose flag.
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
esac

exit 0
