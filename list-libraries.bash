#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

############################################################################################################## Process arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
libraries=( "$script_dir"/racr )
libraries+=( $(find "$script_dir" -type f -name dependencies.txt | sed s/\\/dependencies.txt$// | grep -v /racr$) )

while getopts kl:c: opt
do
	case $opt in
		k)
			for l in ${libraries[@]}
			do
				l=`basename "$l"`
				if [[ ! " ${known_libraries[@]} " =~ "$l" ]]
				then
					known_libraries+=( "$l" )
				fi
			done
			echo ${known_libraries[@]};;
		l)
			for l in ${libraries[@]}
			do
				if [ "$OPTARG" == `basename "$l"` ]
				then
					install_paths+=( "$l" )
				fi
			done
			if [ -z ${install_paths+x} ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR-library !!!" >&2
				exit 2
			else
				echo ${install_paths[@]}
			fi;;
		c)
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
					found=true
					break
				fi
			done
			if [ -z ${found+x} ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] RACR-library directory !!!" >&2
				exit 2
			else
				echo "$absolute_path/dependencies.txt"
			fi;;
		?)
			echo "Usage: -k List all known RACR libraries." >&2
			echo "       -l List absolut paths of the installation directories of a RACR library." >&2
			echo "          Abort with an error if the library is unknown." >&2
			echo "       -c List absolut path to the configuraton file of a RACR library directory." >&2
			echo "          Abort with an error if the library directory is unknown." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
