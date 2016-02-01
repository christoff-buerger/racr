#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

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
		scheme_script=$script_dir/../compile-correct.scm
		java_option="";;
	fail)
		scheme_script=$script_dir/../compile-incorrect.scm
		java_option="-v";;
	esac
	case $1 in
	Larceny)
		larceny --r6rs --path "$script_dir/../tinycpp-racr/larceny-bin:$script_dir/../../../racr/larceny-bin" \
			--program $scheme_script -- $3;;
	Petite)
		petite --libdirs "$script_dir/..:$script_dir/../../.." \
			--program $scheme_script $3;;
	Racket)
		plt-r6rs ++path "$script_dir/../tinycpp-racr/racket-bin" ++path "$script_dir/../../../racr/racket-bin" \
			$scheme_script $3;;
	Guile)
		guile --no-auto-compile -L "$script_dir/../tinycpp-racr/guile-bin" -C "$script_dir/../tinycpp-racr/guile-bin" \
			-L "$script_dir/../../../racr/guile-bin" -C "$script_dir/../../../racr/guile-bin" \
			-s $scheme_script $3;;
	Java_d)
		java -jar $script_dir/../tinycpp-jastadd/tinycpp-declarative.jar $java_option $3;;
	Java_i)
		java -jar $script_dir/../tinycpp-jastadd/tinycpp-iterative.jar $java_option $3;;
	Java_ic)
		java -jar $script_dir/../tinycpp-jastadd/tinycpp-iterative-cached.jar $java_option $3;;
	*)
		echo "Invalid script arguments: Unknown system."
		exit 2
	esac
}

cd $script_dir/correct
rm -rf results-gen
mkdir -p results-gen
for f in *.cpp
do
	echo `pwd`/$f
	cp -p $f results-gen/$f
	run $1 succeed results-gen/$f
done

cd $script_dir/incorrect
rm -rf results-gen
mkdir -p results-gen
for f in *.cpp
do
	echo `pwd`/$f
	cp -p $f results-gen/$f
	run $1 fail results-gen/$f
done
