#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

############################################################################################################## Process arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

while getopts kis: opt
do
	case $opt in
		k)
			echo racket guile larceny petite ironscheme;;
		i)
			for s in racket guile larceny petite
			do
				if which "$s" > /dev/null
				then
					known_systems+=( "$s" )
				fi
			done
			if which IronScheme.Console-v4.exe > /dev/null
			then
				known_systems+=( ironscheme )
			fi
			if [ -z ${known_systems+x} ]
			then
				echo " !!! ERROR: No Scheme system found !!!" >&2
				exit 2
			fi
			echo ${known_systems[@]};;
		s)
			for s in `$script_dir/list-scheme-systems.bash -i`
			do
				if [ "$s" == "$OPTARG" ]
				then
					exit 0
				fi
			done
			echo " !!! ERROR: Unknown [$OPTARG] Scheme system !!!" >&2
			exit 2;;
		?)
			echo "Usage: -k List all known Scheme systems, i.e., the systems officially supported by RACR." >&2
			echo "       -i List all installed and officially supported Scheme systems." >&2
			echo "          Abort with an error if no supported system is installed." >&2
			echo "       -s Ensure a certain system is installed and supported." >&2
			echo "          Abort with an error if not." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
