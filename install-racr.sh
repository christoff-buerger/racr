#!/bin/sh

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

if [ ! $# == 2 ]
then
	echo "Wrong number of arguments - two arguments expected:"
	echo "	(1) RACR source code directory"
	echo "	(2) Scheme distribution directory"
	exit 1
fi
if [ ! -d $1 ]
then
	echo "The given RACR source code directory [$1] does not exist."
	exit 2
fi
if [ ! -d $2 ]
then
	echo "The given Scheme distribution directory [$2] does not exist."
	exit 3
fi

# Declare array of SiPLE source files ordered w.r.t. their compilation dependencies:
declare -a siple_sources=(
	type
	state
	lexer
	ast
	type-coercion
	parser
	access-support
	name-analysis
	type-analysis
	control-flow-analysis
	well-formedness
	interpreter
	main)

# Declare array of Petrinet Language source files ordered w.r.t. their compilation dependencies:
declare -a petrinets_sources=(
	ast
	name-analysis
	well-formedness-analysis
	enabled-analysis
	data-flow-analyses
	ui
	main)

tests=( $(ls $1/tests/*.scm) )

if [ -f $2/bin/plt-r6rs ]
then
	echo "=========================================>>> Install RACR for Racket:"
	
	# Delete old:
	rm -rf $2/collects/racr
	rm -rf $2/collects/racr-test-api
	rm -rf $2/collects/siple
	rm -rf $2/collects/petrinets
	
	# Install new:
	$2/bin/plt-r6rs --all-users --install $1/racr/racr.scm
	$2/bin/plt-r6rs --all-users --install $1/racr/racr-test-api.scm
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		$2/bin/plt-r6rs --all-users --install $1/examples/siple/${siple_sources[i]}.scm
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		$2/bin/plt-r6rs --all-users --install $1/examples/petrinets/${petrinets_sources[i]}.scm
	done
	
	echo "=========================================>>> Run tests to validate installation:"
	for ((i=0; i<${#tests[*]}; i++ )) do
		echo ${tests[i]}
		$2/bin/plt-r6rs ${tests[i]}
	done
else if [ -f $2/larceny ]
then
	echo "=========================================>>> Install RACR for Larceny:"
	
	# Delete old:
	rm -rf $2/lib/racr
	mkdir $2/lib/racr
	rm -rf $2/lib/siple
	mkdir $2/lib/siple
	rm -rf $2/lib/petrinets
	mkdir $2/lib/petrinets
	
	# Copy source files:
	cp $1/racr/racr.scm $2/lib/racr/racr.sls
	cp $1/racr/racr-test-api.scm $2/lib/racr/racr-test-api.sls
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		cp $1/examples/siple/${siple_sources[i]}.scm $2/lib/siple/${siple_sources[i]}.sls
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		cp $1/examples/petrinets/${petrinets_sources[i]}.scm $2/lib/petrinets/${petrinets_sources[i]}.sls
	done
	
	# Create temporary compile script:
	echo "#!r6rs" > racr-compile.sch
	echo "(import (larceny compiler))" >> racr-compile.sch
	echo "(compile-library \"$2/lib/racr/racr.sls\")" >> racr-compile.sch
	echo "(compile-library \"$2/lib/racr/racr-test-api.sls\")" >> racr-compile.sch
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		echo "(compile-library \"$2/lib/siple/${siple_sources[i]}.sls\")" >> racr-compile.sch
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		echo "(compile-library \"$2/lib/petrinets/${petrinets_sources[i]}.sls\")" >> racr-compile.sch
	done
	
	# Execute compile script:
	$2/larceny --r6rs --path $2/lib/racr --program racr-compile.sch
	
	# Delete compile script:
	rm racr-compile.sch
	
	echo "=========================================>>> Run tests to validate installation:"
	for ((i=0; i<${#tests[*]}; i++ )) do
		echo ${tests[i]}
		$2/larceny --r6rs --path $2/lib/racr --program ${tests[i]}
	done
else
	echo "Unknown Scheme distribution - only Larceny and Racket are supported."
fi fi
