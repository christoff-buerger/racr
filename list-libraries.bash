#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

libraries=( "$script_dir/racr" )
libraries+=( $(find "$script_dir" -type f -name racr-library-configuration | sed s/\\/racr-library-configuration$// | grep -v /racr$) )

############################################################################################################## Process arguments:
if [ $# -eq 0 ]
then
	"$script_dir/list-libraries.bash" -h
	exit $?
fi
while getopts kl:c:h opt
do
	case $opt in
		k)
			known_libraries=()
			for l in ${libraries[@]}
			do
				l=`basename "$l"`
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
				if [ "$OPTARG" == `basename "$l"` ]
				then
					echo "$l"
					found=true
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library !!!" >&2
				exit 2
			fi;;
		c)
			found=""
			absolute_path=$(
				if [ -d "$OPTARG" ]
				then
					cd "$OPTARG"
					echo "`pwd`"
				else
					echo ""
				fi
			)
			for l in ${libraries[@]}
			do
				if [ "$absolute_path" == "$l" ]
				then
					echo "$l/racr-library-configuration"
					found=true
					break
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR library directory !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -k List all known RACR libraries (multi-flag)." >&2
			echo "       -l List absolut installation directory paths of a RACR library (multi-parameter)." >&2
			echo "          Abort with an error if the library is unknown." >&2
			echo "       -c List absolut configuraton file path of a RACR library directory (multi-parameter)." >&2
			echo "          Abort with an error if the library directory is unknown." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi
