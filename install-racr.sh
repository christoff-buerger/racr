#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

if [ ! $# == 1 ]
then
	echo "Wrong number of arguments - one argument expected:"
	echo "	(1) Scheme distribution directory"
	exit 1
fi
if [ ! -d $1 ]
then
	echo "The given Scheme distribution directory [$1] does not exist."
	exit 2
fi

# Array of SiPLE source files ordered w.r.t. their compilation dependencies:
declare -a siple_sources=(
	exception-api
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

# Array of SiPLE test programs:
siple_correct=( $(ls ./examples/siple/examples/correct/*.siple) )
siple_incorrect=( $(ls ./examples/siple/examples/incorrect/*.siple) )

# Array of Petri Net Language source files ordered w.r.t. their compilation dependencies:
declare -a petrinets_sources=(
	ast
	access-support
	name-analysis
	composition-analysis
	well-formedness-analysis
	enabled-analysis
	main
	ui)

# Array of RACR tests:
tests=( $(ls ./tests/*.scm) )

if [ -f $1/bin/plt-r6rs ]
then
	echo "=========================================>>> Install RACR for Racket:"
	
	# Delete old:
	rm -rf $1/collects/racr
	rm -rf $1/collects/racr-test-api
	rm -rf $1/collects/siple
	rm -rf $1/collects/petrinets
	
	# Install new:
	$1/bin/plt-r6rs --all-users --install ./racr/racr.scm
	$1/bin/plt-r6rs --all-users --install ./racr/racr-test-api.scm
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		$1/bin/plt-r6rs --all-users --install ./examples/siple/${siple_sources[i]}.scm
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		$1/bin/plt-r6rs --all-users --install ./examples/petrinets/${petrinets_sources[i]}.scm
	done
	
	echo "=========================================>>> Run tests to validate installation:"
	# Basic API tests:
	for ((i=0; i<${#tests[*]}; i++ )) do
		echo ${tests[i]}
		$1/bin/plt-r6rs ${tests[i]}
	done
	# Testing state machine example:
	echo ./examples/state-machines/state-machines.scm
	$1/bin/plt-r6rs ./examples/state-machines/state-machines.scm
	# Testing Petri net example:
	echo ./examples/petrinets/examples/purchase-processing.scm
	$1/bin/plt-r6rs ./examples/petrinets/examples/purchase-processing.scm
	echo ./examples/petrinets/examples/runtime-structure-example-slide.scm
	$1/bin/plt-r6rs ./examples/petrinets/examples/runtime-structure-example-slide.scm
	# Testing SiPLE example:
	for ((i=0; i<${#siple_correct[*]}; i++ )) do
		echo ${siple_correct[i]}
		$1/bin/plt-r6rs ./examples/siple/examples/run-correct.scm ${siple_correct[i]}
	done
	for ((i=0; i<${#siple_incorrect[*]}; i++ )) do
		echo ${siple_incorrect[i]}
		$1/bin/plt-r6rs ./examples/siple/examples/run-incorrect.scm ${siple_incorrect[i]}
	done
else if [ -f $1/larceny ]
then
	echo "=========================================>>> Install RACR for Larceny:"
	
	# Delete old:
	rm -rf $1/lib/racr
	mkdir $1/lib/racr
	rm -rf $1/lib/siple
	mkdir $1/lib/siple
	rm -rf $1/lib/petrinets
	mkdir $1/lib/petrinets
	
	# Copy source files:
	cp ./racr/racr.scm $1/lib/racr/racr.sls
	cp ./racr/racr-test-api.scm $1/lib/racr/racr-test-api.sls
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		cp ./examples/siple/${siple_sources[i]}.scm $1/lib/siple/${siple_sources[i]}.sls
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		cp ./examples/petrinets/${petrinets_sources[i]}.scm $1/lib/petrinets/${petrinets_sources[i]}.sls
	done
	
	# Create temporary compile script:
	echo "#!r6rs" > racr-compile.sch
	echo "(import (larceny compiler))" >> racr-compile.sch
	echo "(compile-library \"$1/lib/racr/racr.sls\")" >> racr-compile.sch
	echo "(compile-library \"$1/lib/racr/racr-test-api.sls\")" >> racr-compile.sch
	for (( i=0; i<${#siple_sources[*]}; i++ )) do
		echo "(compile-library \"$1/lib/siple/${siple_sources[i]}.sls\")" >> racr-compile.sch
	done
	for (( i=0; i<${#petrinets_sources[*]}; i++ )) do
		echo "(compile-library \"$1/lib/petrinets/${petrinets_sources[i]}.sls\")" >> racr-compile.sch
	done
	
	# Execute compile script:
	$1/larceny --r6rs --path $1/lib/racr --program racr-compile.sch
	
	# Delete compile script:
	rm racr-compile.sch
	
	echo "=========================================>>> Run tests to validate installation:"
	# Basic API tests:
	for ((i=0; i<${#tests[*]}; i++ )) do
		echo ${tests[i]}
		$1/larceny --r6rs --path $1/lib/racr --program ${tests[i]}
	done
	# Testing state machine example:
	echo ./examples/state-machines/state-machines.scm
	$1/larceny --r6rs --path $1/lib/racr --program ./examples/state-machines/state-machines.scm
else
	echo "Unknown Scheme distribution - only Larceny and Racket are supported."
fi fi
