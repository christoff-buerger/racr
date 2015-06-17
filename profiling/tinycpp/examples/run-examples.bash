#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

if (( $# < 1 ))
then
	echo "Usage: ./run-examples system"
	echo "Help: Execute the tests using a certain system."
	echo "      Supported systems are Larceny, Racket, Petite, Java_i, Java_ic."
	exit 2
fi

run(){
	case $2 in
	succeed)
		scheme_script=$old_pwd/../compile-correct.scm
		java_option="";;
	fail)
		scheme_script=$old_pwd/../compile-incorrect.scm
		java_option="-v";;
	esac
	case $1 in
	Larceny)
		larceny --r6rs --path "$old_pwd/../tinycpp-racr/larceny-bin:$old_pwd/../../../racr/larceny-bin" \
			--program $scheme_script -- $3;;
	Petite)
		petite --libdirs "$old_pwd/..:$old_pwd/../../.." \
			--program $scheme_script $3;;
	Racket)
		plt-r6rs ++path "$old_pwd/../tinycpp-racr/racket-bin" ++path "$old_pwd/../../../racr/racket-bin" \
			$scheme_script $3;;
	Guile)
		guile --no-auto-compile -L "$old_pwd/../tinycpp-racr/guile-bin" -C "$old_pwd/../tinycpp-racr/guile-bin" \
			-L "$old_pwd/../../../racr/guile-bin" -C "$old_pwd/../../../racr/guile-bin" \
			-s $scheme_script $3;;
	Java_d)
		java -jar $old_pwd/../tinycpp-jastadd/tinycpp-declarative.jar $java_option $3;;
	Java_i)
		java -jar $old_pwd/../tinycpp-jastadd/tinycpp-iterative.jar $java_option $3;;
	Java_ic)
		java -jar $old_pwd/../tinycpp-jastadd/tinycpp-iterative-cached.jar $java_option $3;;
	*)
		echo "Invalid script arguments: Unknown system."
		exit 2
	esac
}

cd $old_pwd/correct
rm -rf results-gen
mkdir -p results-gen
for f in *.cpp
do
	echo `pwd`/$f
	cp -p $f results-gen/$f
	run $1 succeed results-gen/$f
done

cd $old_pwd/incorrect
rm -rf results-gen
mkdir -p results-gen
for f in *.cpp
do
	echo `pwd`/$f
	cp -p $f results-gen/$f
	run $1 fail results-gen/$f
done
