#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

results=() # Collect results and only print them if ALL arguments are valid.

############################################################################################################## Process arguments:
if [ $# -eq 0 ]
then
	"$script_dir/list-scheme-systems.bash" -h
	exit $?
fi

while getopts kis:h opt
do
	case $opt in
		k)
			for s in chez guile racket larceny sagittarius ypsilon ironscheme
			do
				results+=( "$s" )
			done
			;;
		i)
			found=""
			for s in chez guile racket larceny sagittarius ypsilon
			do
				if command -v "$s" > /dev/null
				then
					results+=( "$s" )
					found="true"
				fi
			done
			if command -v IronScheme.Console-v4.exe > /dev/null
			then
				results+=( "ironscheme" )
				found="true"
			fi
			if [ -z "$found" ]
			then
				echo " !!! ERROR: No Scheme system found !!!" >&2
				exit 2
			fi
			;;
		s)
			found=""
			for s in $( "$script_dir/list-scheme-systems.bash" -i )
			do
				if [ "$OPTARG" == "$s"  ]
				then
					found="true"
					break
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] Scheme system selected via -s parameter !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -k List all Scheme systems officially supported by RACR (multi-flag)." >&2
			echo "       -i List all installed and officially supported Scheme systems (multi-flag)." >&2
			echo "          Abort with an error if no supported system is installed." >&2
			echo "       -s Ensure a certain system is installed and supported (multi-parameter)." >&2
			echo "          Abort with an error if not." >&2
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

################################################################################################################## Print results:
for r in "${results[@]}"
do
	echo "$r"
done

exit 0
