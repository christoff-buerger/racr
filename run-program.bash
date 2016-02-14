#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

while getopts s:e:l: opt
do
	case $opt in
		s)
			if [ -z ${selected_system+x} ]
			then
				"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
				if [ $? -eq 0 ]
				then
					selected_system="$OPTARG"
				else
					exit 2
				fi
			else
				echo " !!! ERROR: Several Scheme systems for execution selected via -s flag !!!" >&2
				exit 2
			fi;;
		e)
			if [ -z ${to_execute+x} ]
			then
				to_execute="$OPTARG"
				if [ -f `dirname "$to_execute"`/dependencies.txt ]
				then
					library_to_use=`dirname "$to_execute"`
				fi
			else
				echo " !!! ERROR: Several programs to execute specified via -e flag !!!" >&2
				exit 2
			fi;;
		l)
			if [ -z ${library_to_use+x} ]
			then
				library_to_use="$OPTARG"
				if [ ! -f "$library_to_use/dependencies.txt" ]
				then
					echo " !!! ERROR: [$library_to_use] is not a RACR library directory !!!" >&2
					exit 2
				fi
			else
				echo " !!! ERROR: Several libraries to use specified, either via -l flag or implicitly" >&2
				echo "            because the program to execute is in a RACR library directory !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (`"$script_dir/list-scheme-systems.bash" -i`)." >&2
			echo "       -e Scheme program to execute." >&2
			echo "       -c RACR library to use (implicitly set if the program" >&2
			echo "          to execute is in a RACR library directory)." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ -z ${to_execute+x} ]
then
	echo " !!! ERROR: No Scheme program to execute specified via -e flag !!!" >&2
	exit 2
fi

if [ -z ${selected_system+x} ]
then
	echo " !!! ERROR: No Scheme system for execution selected via -s flag !!!" >&2
	exit 2
fi

if [ -z ${library_to_use+x} ]
then
	required_libraries=( "$script_dir/racr" )
else
	configuration_to_parse="$library_to_use/dependencies.txt"
	. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
	if [[ ! " ${supported_systems[@]} " =~ "$selected_system" ]]
	then
		echo " !!! ERROR: Scheme system [$selected_system] not supported by the program !!!" >&2
		exit 2
	fi
fi

################################################################################################################ Execute program:
case $selected_system in
	racket)
		libs=""
		for l in ${required_libraries[@]}
		do
			libs+=" ++path $l/racket-bin"
		done
		plt-r6rs $libs "$to_execute" $*;;
	guile)
		llibs=""
		clibs=""
		for l in ${required_libraries[@]}
		do
			llibs+=" -L $l/guile-bin"
			clibs+=" -C $l/guile-bin"
		done
		guile --no-auto-compile $llibs $clibs -s "$to_execute" $*;;
	larceny)
		libs=""
		for l in ${required_libraries[@]}
		do
			libs+=":$l/larceny-bin"
		done
		if [ ! -z "$libs" ]
		then
			libs="--path ${libs:1}"
		fi
		arguments=""
		if [ "$#" -ge 1 ]
		then
			arguments="-- $*"
		fi
		larceny --r6rs $libs --program "$to_execute" $arguments;;
	petite)
		libs=""
		for l in ${required_libraries[@]}
		do
			libs+=":$l/.."
		done
		if [ ! -z "$libs" ]
		then
			libs="--libdirs ${libs:1}"
		fi
		petite $libs --program "$to_execute" $*;;
	ironscheme)
		mono "$script_dir/racr-net/ironscheme-bin/IronScheme.Console-v4.exe" "$to_execute" $*;;
esac
