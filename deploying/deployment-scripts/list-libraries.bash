#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

libraries_relative=( "$script_dir/../../racr" )
libraries_relative+=( $( find "$script_dir/../.." -type f -name racr-library-configuration | \
	sort | sed s/\\/racr-library-configuration$// | grep -v /racr$ ) )
libraries=()
for l in ${libraries_relative[@]}
do
	libraries+=( "$( cd "$l" && pwd )" )
done

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
			known_libraries=()
			for l in ${libraries[@]}
			do
				l="$( basename "$l" )"
				if [[ ! " ${known_libraries[@]} " =~ "$l" ]]
				then
					known_libraries+=( "$l" )
					echo "$l"
				fi
			done;;
		l)
			found=""
			for l in ${libraries[@]}
			do
				if [ "$OPTARG" == "$( basename "$l" )" ]
				then
					found=true
					echo "$l"
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library !!!" >&2
				exit 2
			fi;;
		i)
			for k in `"$script_dir/list-libraries.bash" -k`
			do
				"$script_dir/list-libraries.bash" -l "$k"
			done;;
		c)
			found=""
			if [ -d "$OPTARG" ]
			then
				absolute_path="$( cd "$OPTARG" && pwd )"
				for l in ${libraries[@]}
				do
					if [ "$absolute_path" == "$l" ]
					then
						found=true
						echo "$l/racr-library-configuration"
						break
					fi
				done
			fi
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library directory !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -k List all known RACR libraries (multi-flag)." >&2
			echo "       -l List all directories of a RACR library (multi-parameter)." >&2
			echo "          The listed paths are absolute." >&2
			echo "          Abort with an error if the library is unknown." >&2
			echo "       -i List all directories of all known RACR libraries (multi-flag)." >&2
			echo "          The listed paths are absolute." >&2
			echo "       -c List configuration file of a RACR library directory (multi-parameter)." >&2
			echo "          The listed path is absolute." >&2
			echo "          Abort with an error if the library directory is unknown." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$@] command line arguments !!!" >&2
	exit 2
fi

exit 0
