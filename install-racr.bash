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

if which plt-r6rs > /dev/null
then
	echo "=========================================>>> Compile RACR for Racket:"
	
	for l in ${libraries[@]}
	do
		ll=`echo $l | rev | cut -d/ -f1 | rev` # extract last file part of string
		cd $l
		rm -rf racket-bin
		mkdir -p racket-bin/$ll
		cat dependencies.txt | while read line
		do
			plt-r6rs ++path ${libraries[0]}/racket-bin --install --collections $l/racket-bin $line.scm
		done
	done
fi

if which larceny > /dev/null
then
	echo "=========================================>>> Compile RACR for Larceny:"
	
	# Create compile script
	cd $old_pwd
	echo "#!r6rs" > compile-stale
	echo "(import (larceny compiler))" >> compile-stale
	echo "(compile-stale-libraries)" >> compile-stale
	
	# Compile libraries:
	for l in ${libraries[@]}
	do
		ll=`echo $l | rev | cut -d/ -f1 | rev` # extract last file part of string
		cd $l
		rm -rf larceny-bin
		mkdir -p larceny-bin/$ll
		for f in *.scm
		do
			cp -p $f larceny-bin/$ll/${f%.*}.sls
		done
		cd larceny-bin/$ll
		cp -p $old_pwd/compile-stale .
		larceny --r6rs --path "${libraries[0]}/larceny-bin:./.." --program compile-stale
		rm compile-stale
	done
	
	# Delete compile script:
	cd $old_pwd
	rm compile-stale
fi

cd $old_pwd
