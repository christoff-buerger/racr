#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

# Array of libraries to compile; First element must be RACR:
declare -a libraries=(
	$old_pwd/racr
	# Find all directories with 'dependencies.txt'; Each such directory contains Scheme libraries:
	$(find $old_pwd -type f -name dependencies.txt | sed s/\\/dependencies.txt$// | grep -v /racr$) )

if (( $# > 0 ))
then
	new_libraries=( )
	for l in ${libraries[@]}
	do
		for name in $*
		do
			if `echo "$l" | grep -q "/$name"$`
			then
				new_libraries+=( "$l" )
				break
			fi
		done
	done
	libraries=( ${new_libraries[@]} )
fi

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
			local_systems=( racket larceny )
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
			local_systems+=( "$local_dir/$line" )
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

if which plt-r6rs > /dev/null
then
	echo "=========================================>>> Compile for Racket:"
	
	for l in ${libraries[@]}
	do
		rm -rf "$l/racket-bin"
		mkdir -p "$l/racket-bin/`basename "$l"`"
		read_dependencies "$l/dependencies.txt"
		lib_path=""
		for x in ${local_libraries[@]}
		do
			lib_path+=" ++path $x/racket-bin"
		done
		for x in ${local_sources[@]}
		do
			plt-r6rs $lib_path --install --collections "$l/racket-bin" "$x.scm"
		done
	done
fi

if which guild > /dev/null
then
	echo "==========================================>>> Compile for Guile:"
	for l in ${libraries[@]}
	do
		l_bin="$l/guile-bin"
		l_lib="$l_bin/`basename "$l"`"
		rm -rf "$l_bin"
		mkdir -p "$l_lib"
		read_dependencies "$l/dependencies.txt"
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
	done
fi

if which larceny > /dev/null
then
	echo "=========================================>>> Compile for larceny:"
	
	# Create compile script:
	cd $old_pwd
	echo "#!r6rs" > compile-stale
	echo "(import (rnrs) (larceny compiler))" >> compile-stale
	echo "(compiler-switches (quote fast-safe))" >> compile-stale # Just for optimisation. Even more aggressive: fast-unsafe
	echo "(compile-stale-libraries)" >> compile-stale
	
	# Compile libraries:
	for l in ${libraries[@]}
	do
		ll=`echo $l | rev | cut -d/ -f1 | rev` # Extract last file part of string
		cd $l
		rm -rf larceny-bin
		mkdir -p larceny-bin/$ll
		read_dependencies "$l/dependencies.txt"
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
	done
	
	# Delete compile script:
	cd $old_pwd
	rm compile-stale
fi

cd $old_pwd
