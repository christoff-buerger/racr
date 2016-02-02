#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
known_systems=( racket guile larceny )
selected_systems=()
known_libraries=( "$script_dir"/racr )
known_libraries+=( $(find "$script_dir" -type f -name dependencies.txt | sed s/\\/dependencies.txt$// | grep -v /racr$) )
selected_libraries=()

while getopts s:i: opt
do
	case $opt in
		s)
			if [[ " ${known_systems[@]} " =~ " ${OPTARG} " ]]
			then
				if which "${OPTARG}" > /dev/null
				then
					selected_systems+=( "$OPTARG" )
				else
					echo " !!! ERROR: Scheme system [$OPTARG] not installed !!!" >&2
					exit 2
				fi
			else
				echo " !!! ERROR: Unknown [$OPTARG] Scheme system !!!" >&2
				exit 2
			fi;;
		i)
			found=""
			for l in ${known_libraries[@]}
			do
				if `echo "$l" | grep -q "/${OPTARG}"$`
				then
					selected_libraries+=( "$l" )
					found=true
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] library !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (${known_systems[@]})." >&2
			echo "          Several Scheme systems can be set. If no system is selected," >&2
			echo "          the libraries are installed for all available systems." >&2
			echo "       -i RACR libraries to install." >&2
			echo "          Several libraries can be set. If no library is selected," >&2
			echo "          all libraries are installed." >&2
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$selected_systems" ]
then
	for s in ${known_systems[@]}
	do
		if which "$s" > /dev/null
		then
			selected_systems+=( "$s" )
		fi
	done
	if [ -z "$selected_systems" ]
	then
		echo " !!! ERROR: No Scheme system found !!!" >&2
		exit 2
	fi
fi

if [ -z "$selected_libraries" ]
then	
	selected_libraries=${known_libraries[@]}
fi

############################################################################################################## Install libraries:
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "=========================================>>> Compile for Racket:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "racket" ]]
			then
			rm -rf "$l/racket-bin"
			mkdir -p "$l/racket-bin/`basename "$l"`"
			lib_path=""
			for x in ${required_libraries[@]}
			do
				lib_path+=" ++path $x/racket-bin"
			done
			for x in ${required_sources[@]}
			do
				plt-r6rs $lib_path --install --collections "$l/racket-bin" "$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "guile" ]]
then
	echo "==========================================>>> Compile for Guile:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "guile" ]]
		then
			l_bin="$l/guile-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path="--load-path=$l_bin"
			for x in ${required_libraries[@]}
			do
				lib_path+=" --load-path=$x/guile-bin"
			done
			for x in ${required_sources[@]}
			do
				cp -p "$x.scm" "$l_lib"
				x=`basename "$x"`
				guild compile $lib_path --output="$l_lib/$x.go" "$l_lib/$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "larceny" ]]
then
	# Use subshell for local directory changes via cd:
	(
	echo "=========================================>>> Compile for Larceny:"
	# Create compile script:
	cd "$script_dir"
	echo "#!r6rs" > compile-stale
	echo "(import (rnrs) (larceny compiler))" >> compile-stale
	echo "(compiler-switches (quote fast-safe))" >> compile-stale # Just for optimisation. Even more aggressive: fast-unsafe
	echo "(compile-stale-libraries)" >> compile-stale
	# Compile libraries:
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "larceny" ]]
		then
			ll=`echo "$l" | rev | cut -d/ -f1 | rev` # Extract last file part of string
			cd "$l"
			rm -rf larceny-bin
			mkdir -p "larceny-bin/$ll"
			lib_path=".."
			for x in ${required_libraries[@]}
			do
				lib_path+=":$x/larceny-bin"
			done
			for x in ${required_sources[@]} 
			do
				cp -p "$x.scm" "larceny-bin/$ll/`basename "$x"`.sls"
			done
			cd "larceny-bin/$ll"
			cp -p "$script_dir/compile-stale" .
			larceny --r6rs --path $lib_path --program compile-stale
			rm compile-stale
		fi
	done
	# Delete compile script:
	cd "$script_dir"
	rm compile-stale
	)
fi
