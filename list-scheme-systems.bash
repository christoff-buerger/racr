#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

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
			for s in racket guile larceny petite sagittarius ironscheme
			do
				echo "$s"
			done;;
		i)
			found=""
			for s in racket guile larceny petite sagittarius
			do
				if which "$s" > /dev/null
				then
					echo "$s"
					found=true
				fi
			done
			if which IronScheme.Console-v4.exe > /dev/null
			then
				echo ironscheme
				found=true
			fi
			if [ -z "$found" ]
			then
				echo " !!! ERROR: No Scheme system found !!!" >&2
				exit 2
			fi;;
		s)
			found=""
			for s in `$script_dir/list-scheme-systems.bash -i`
			do
				if [ "$OPTARG" == "$s"  ]
				then
					found=true
					break
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] Scheme system !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -k List all Scheme systems officially supported by RACR (multi-flag)." >&2
			echo "       -i List all installed and officially supported Scheme systems (multi-flag)." >&2
			echo "          Abort with an error if no supported system is installed." >&2
			echo "       -s Ensure a certain system is installed and supported (multi-parameter)." >&2
			echo "          Abort with an error if not." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))
if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi
