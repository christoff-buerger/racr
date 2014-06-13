#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

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
	main
	test-api)

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

old_pwd=`pwd`

if which plt-r6rs
then
	echo "=========================================>>> Install RACR for Racket:"
	
	cd racr
	rm -rf racket-bin
	mkdir racket-bin
	plt-r6rs --install --collections ./racket-bin racr.scm
	plt-r6rs --install --collections ./racket-bin racr-test-api.scm
	
	# Delete old:
	#rm -rf $1/collects/racr
	#rm -rf $1/collects/racr-test-api
	#rm -rf $1/collects/siple
	#rm -rf $1/collects/petrinets
	
	# Install new:
	#$1/bin/plt-r6rs --all-users --install ./racr/racr.scm
	#$1/bin/plt-r6rs --all-users --install ./racr/racr-test-api.scm
	#for f in ${siple_sources[@]}
	#do
	#	$1/bin/plt-r6rs --all-users --install ./examples/siple/${f}.scm
	#done
	#for f in ${petrinets_sources[@]}
	#do
	#	$1/bin/plt-r6rs --all-users --install ./examples/petrinets/${f}.scm
	#done
	
	#echo "=========================================>>> Run tests to validate installation:"
	# Basic API tests:
	#for f in ${tests[@]}
	#do
	#	echo $f
	#	$1/bin/plt-r6rs $f
	#done
	# Testing state machine example:
	#echo ./examples/state-machines/state-machines.scm
	#$1/bin/plt-r6rs ./examples/state-machines/state-machines.scm
	# Testing Petri net example:
	#echo ./examples/petrinets/examples/purchase-processing.scm
	#$1/bin/plt-r6rs ./examples/petrinets/examples/purchase-processing.scm
	#echo ./examples/petrinets/examples/runtime-structure-example-slide.scm
	#$1/bin/plt-r6rs ./examples/petrinets/examples/runtime-structure-example-slide.scm
	# Testing SiPLE example:
	#for f in ${siple_correct[@]}
	#do
	#	echo $f
	#	$1/bin/plt-r6rs ./examples/siple/examples/run-correct.scm $f
	#done
	#for f in ${siple_incorrect[@]}
	#do
	#	echo $f
	#	$1/bin/plt-r6rs ./examples/siple/examples/run-incorrect.scm $f
	#done
fi

cd $old_pwd

if which larceny
then
	echo "=========================================>>> Install RACR for Larceny:"
	
	cd racr
	
	# Delete old binaries:
	rm -rf larceny-bin
	mkdir larceny-bin
	
	# Copy source files:
	cp -p racr.scm larceny-bin/racr.sls
	cp -p racr-test-api.scm larceny-bin/racr-test-api.sls
	
	# Create compile script
	cd larceny-bin	
	echo "#!r6rs" > compile-stale
	echo "(import (larceny compiler))" >> compile-stale
	echo "(compile-stale-libraries)" >> compile-stale
	
	# Execute compile script:
	larceny --r6rs --path "." --program compile-stale
	
	# Delete compile script:
	rm compile-stale
	
	# Delete old:
	#rm -rf $1/lib/racr
	#mkdir $1/lib/racr
	#rm -rf $1/lib/siple
	#mkdir $1/lib/siple
	#rm -rf $1/lib/petrinets
	#mkdir $1/lib/petrinets
	
	# Copy source files:
	#cp ./racr/racr.scm $1/lib/racr/racr.sls
	#cp ./racr/racr-test-api.scm $1/lib/racr/racr-test-api.sls
	#for f in ${siple_sources[@]}
	#do
	#	cp ./examples/siple/${f}.scm $1/lib/siple/${f}.sls
	#done
	#for f in ${petrinets_sources[@]}
	#do
	#	cp ./examples/petrinets/${f}.scm $1/lib/petrinets/${f}.sls
	#done
	
	# Create temporary compile script:
	#echo "#!r6rs" > racr-compile.sch
	#echo "(import (larceny compiler))" >> racr-compile.sch
	#echo "(compile-library \"$1/lib/racr/racr.sls\")" >> racr-compile.sch
	#echo "(compile-library \"$1/lib/racr/racr-test-api.sls\")" >> racr-compile.sch
	#for f in ${siple_sources[@]}
	#do
	#	echo "(compile-library \"$1/lib/siple/${f}.sls\")" >> racr-compile.sch
	#done
	#for f in ${petrinets_sources[@]}
	#do
	#	echo "(compile-library \"$1/lib/petrinets/${f}.sls\")" >> racr-compile.sch
	#done
	
	# Execute compile script:
	#$1/larceny --r6rs --path $1/lib/racr --program racr-compile.sch
	
	# Delete compile script:
	#rm racr-compile.sch
	
	#echo "=========================================>>> Run tests to validate installation:"
	# Basic API tests:
	#for f in ${tests[@]}
	#do
	#	echo $f
	#	$1/larceny --r6rs --path $1/lib/racr --program $f
	#done
	# Testing state machine example:
	#echo ./examples/state-machines/state-machines.scm
	#$1/larceny --r6rs --path $1/lib/racr --program ./examples/state-machines/state-machines.scm
fi

cd $old_pwd

