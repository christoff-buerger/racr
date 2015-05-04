#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
while getopts s:d:i:t: opt
do
	case $opt in
		s)	system="$OPTARG";;
		d)	diagram="$OPTARG";;
		i)	input="$OPTARG";;
		t)	trace="$OPTARG";;
		?)
			echo "Usage: -s Scheme system (racket, larceny, petite))"
			echo "       -d Activity diagram"
			echo "       -i Activity diagram input"
			echo "       -t Execution trace"
			exit 2
	esac
done
shift $(( OPTIND - 1 ))

if [ -z "$system" ]
then
	echo " !!! ERROR: No Scheme system given !!!" >&2
	exit 2
fi
if [ -z "$diagram" ]
then
	echo " !!! ERROR: No activity diagram to interpret given !!!" >&2
	exit 2
fi
if [ -z "$trace" ]
then
	echo " !!! ERROR: No tracing log given !!!" >&2
	exit 2
fi

if [ -z "$input" ]
then
	input=":"
fi

############################################################################### Configure temporary resources & execution script:
old_pwd=`pwd`

my_exit(){
	cd $old_pwd
	rm script.scm 
	rm tmp-trace.trace
	exit 0
}
trap 'my_exit' 1 2 3 9 15

echo "#!r6rs" > script.scm
echo "(import (rnrs) (ttc-2015-fuml-activity-diagrams user-interface))" >> script.scm
echo "(define diagram (cadr (command-line)))" >> script.scm
echo "(define input (caddr (command-line)))" >> script.scm
echo "(define trace (cadddr (command-line)))" >> script.scm
echo '(set! input (if (string=? input ":") #f input))' >> script.scm
echo "(run-activity-diagram diagram input trace)" >> script.scm

####################################################################################################### Execute activity diagram:
case "$system" in
larceny)
	larceny --r6rs --path "../../racr/larceny-bin:../atomic-petrinets/larceny-bin:larceny-bin" \
		--program script.scm -- $diagram $input tmp-trace.trace;;
racket)
	plt-r6rs ++path "../../racr/racket-bin" ++path "../atomic-petrinets/racket-bin" ++path "racket-bin" \
		script.scm $diagram $input tmp-trace.trace;;
petite)
	petite --libdirs "../..:.." \
		--program script.scm $diagram $input tmp-trace.trace;;
*)
	echo " !!! ERROR: Unknown Scheme system [$system] !!!"
	touch tmp-trace.trace
	my_exit;;
esac

############################################################################################################# Save/compare trace:
if [ -e "$trace" ]
then
	diff_result=`diff --text trace.trace "$trace"`
	if [ "$diff_result" != "" ]
	then
		echo " !!! RESULT ERROR !!!"
	fi
else
	cp -p tmp-trace.trace "$trace"
fi

my_exit
