#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
old_pwd=`pwd`
supported_systems=( racket guile larceny )
selected_systems=()
supported_libraries=( "$old_pwd"/racr )
supported_libraries+=( $(find "$old_pwd" -type f -name dependencies.txt | sed s/\\/dependencies.txt$// | grep -v /racr$) )
selected_libraries=()

while getopts s:i: opt
do
	case $opt in
		s)
			if [[ " ${supported_systems[@]} " =~ " ${OPTARG} " ]]
			then
				if which "${OPTARG}" > /dev/null
				then
					selected_systems+=( "$OPTARG" )
				else
					echo " !!! ERROR: [$OPTARG] not installed !!!" >&2
					exit 2
				fi
			else
				echo " !!! ERROR: Unknown [$OPTARG] Scheme system !!!" >&2
				exit 2
			fi;;
		i)
			found=""
			for l in ${supported_libraries[@]}
			do
				if `echo "$l" | grep -q "/${OPTARG}"$`
				then
					selected_libraries+=( "$l" )
					found=true	
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [${OPTARG}] library !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (${supported_systems[@]})"
			echo "       -i Module to install"
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$selected_systems" ]
then
	for s in ${supported_systems[@]}
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
	selected_libraries=${supported_libraries[@]}
fi

####################################################################################################### Define support functions:
read_dependencies(){
	mode=initial
	local_dir=`dirname "$1"`
	local_systems=()
	local_libraries=()
	local_sources=()
	while read line
	do
		case $mode in
		initial)
			if [ "$line" = "@systems:" ]
			then
				mode=systems
				continue
			fi
			local_systems=${selected_systems[@]}
			if [ "$line" = "@libraries:" ]
			then
				mode=libraries
				continue
			fi
			if [ "$line" = "@sources:" ]
			then
				mode=sources
				continue
			fi
			mode=sources
			local_sources+=( "$local_dir/$line" );;
		systems)
			if [ "$line" = "@libraries:" ]
			then
				mode=libraries
				continue
			fi
			if [ "$line" = "@sources:" ]
			then
				mode=sources
				continue
			fi
			if [[ " ${selected_systems[@]} " =~ "$line" ]]
			then
				local_systems+=( "$line" )
			fi
			continue;;
		libraries)
			if [ "$line" = "@sources:" ]
			then
				mode=sources
				continue
			fi
			local_libraries+=( "$local_dir/$line" );;
		sources)
			local_sources+=( "$local_dir/$line" );;
		esac
	done < "$1"
}

############################################################################################################## Install libraries:
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "=========================================>>> Compile for Racket:"
	
	for l in ${selected_libraries[@]}
	do
		read_dependencies "$l/dependencies.txt"
		if [[ " ${local_systems[@]} " =~ "racket" ]]
			then
			rm -rf "$l/racket-bin"
			mkdir -p "$l/racket-bin/`basename "$l"`"
			lib_path=""
			for x in ${local_libraries[@]}
			do
				lib_path+=" ++path $x/racket-bin"
			done
			for x in ${local_sources[@]}
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
		read_dependencies "$l/dependencies.txt"
		if [[ " ${local_systems[@]} " =~ "guile" ]]
		then
			l_bin="$l/guile-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path="--load-path=$l_bin"
			for x in ${local_libraries[@]}
			do
				lib_path+=" --load-path=$x/guile-bin"
			done
			for x in ${local_sources[@]}
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
	echo "=========================================>>> Compile for larceny:"
	
	# Create compile script:
	cd $old_pwd
	echo "#!r6rs" > compile-stale
	echo "(import (rnrs) (larceny compiler))" >> compile-stale
	echo "(compiler-switches (quote fast-safe))" >> compile-stale # Just for optimisation. Even more aggressive: fast-unsafe
	echo "(compile-stale-libraries)" >> compile-stale
	
	# Compile libraries:
	for l in ${selected_libraries[@]}
	do
		read_dependencies "$l/dependencies.txt"
		if [[ " ${local_systems[@]} " =~ "larceny" ]]
		then
			ll=`echo $l | rev | cut -d/ -f1 | rev` # Extract last file part of string
			cd $l
			rm -rf larceny-bin
			mkdir -p larceny-bin/$ll
			lib_path=".."
			for x in ${local_libraries[@]}
			do
				lib_path+=":$x/larceny-bin"
			done
			for f in *.scm
			do
				cp -p $f larceny-bin/$ll/${f%.*}.sls
			done
			cd larceny-bin/$ll
			cp -p $old_pwd/compile-stale .
			larceny --r6rs --path $lib_path --program compile-stale
			rm compile-stale
		fi
	done
	
	# Delete compile script:
	cd $old_pwd
	rm compile-stale
fi

cd $old_pwd
