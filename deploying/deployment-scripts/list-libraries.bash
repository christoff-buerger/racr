#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

libraries_relative=( "$script_dir/../../racr" )
mapfile -O 1 -t libraries_relative < <(
	find "$script_dir/../.." -type f -name racr-library-configuration \
	| sort \
	| sed s/\\/racr-library-configuration$// \
	| grep -v /racr$ \
	|| kill -13 $$ )
libraries=()
for l in "${libraries_relative[@]}"
do
	libraries+=( "$( cd "$l" && pwd )" )
done

results=() # Collect results and only print them if ALL arguments are valid.

############################################################################################################## Process arguments:
if [ $# -eq 0 ]
then
	"$script_dir/list-libraries.bash" -h
	exit $?
fi

while getopts kl:ic:h opt
do
	case $opt in
		k)
			declare -A known_libraries
			known_libraries=()
			for l in "${libraries[@]}"
			do
				l="$( basename "$l" )"
				if [[ ! -v "known_libraries[$l]" ]]
				then
					known_libraries["$l"]="$l"
					results+=( "$l" )
				fi
			done
			;;
		l)
			found=""
			for l in "${libraries[@]}"
			do
				if [ "$OPTARG" == "$( basename "$l" )" ]
				then
					found="true"
					results+=( "$l" )
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library !!!" >&2
				exit 64
			fi
			;;
		i)
			results+=( "${libraries[@]}" )
			;;
		c)
			found=""
			if [ -d "$OPTARG" ]
			then
				absolute_path="$( cd "$OPTARG" && pwd )"
				for l in "${libraries[@]}"
				do
					if [ "$absolute_path" == "$l" ]
					then
						found="true"
						results+=( "$l/racr-library-configuration" )
						break
					fi
				done
			fi
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library directory !!!" >&2
				exit 64
			fi
			;;
		h|?)
			echo "Usage: -k List all known RACR libraries (optional multi-flag)." >&2
			echo "       -l List all directories of a RACR library (optional multi-parameter)." >&2
			echo "          The listed paths are absolute." >&2
			echo "          Abort with an error if the library is unknown." >&2
			echo "       -i List all directories of all known RACR libraries (optional multi-flag)." >&2
			echo "          The listed paths are absolute." >&2
			echo "       -c List configuration file of a RACR library directory (optional multi-parameter)." >&2
			echo "          The listed path is absolute." >&2
			echo "          Abort with an error if the library directory is unknown." >&2
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

################################################################################################################## Print results:
for r in "${results[@]}"
do
	echo "$r"
done

exit 0
