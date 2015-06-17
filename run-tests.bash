#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
supported_systems=( racket guile larceny petite )
selected_systems=()
while getopts s: opt
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
		?)
			echo "Usage: -s Scheme system (${supported_systems[@]})"	
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

###################################################################################################### Define exeution functions:
old_pwd=`pwd`

begin_run(){
	echo `pwd`/$1
}

racket_run(){
	if [[ " ${selected_systems[@]} " =~ "racket" ]]
	then
		printf " Racket"
		if [ "$1" == "" ]
		then
			libs="++path $old_pwd/racr/racket-bin"
		else
			libs="++path $old_pwd/racr/racket-bin ++path $1"
		fi
		plt-r6rs $libs $2
	fi
}

guile_run(){
	if [[ " ${selected_systems[@]} " =~ "guile" ]]
	then
		printf " Guile"
		if [ "$1" == "" ]
		then
			llibs="-L $old_pwd/racr/guile-bin"
			clibs="-C $old_pwd/racr/guile-bin"
		else
			llibs="-L $old_pwd/racr/guile-bin -L $1"
			clibs="-C $old_pwd/racr/guile-bin -C $1"
		fi
		guile --no-auto-compile $llibs $clibs -s $2
	fi
}

larceny_run(){
	if [[ " ${selected_systems[@]} " =~ "larceny" ]]
	then
		printf " Larceny"
		if [ "$1" == "" ]
		then
			libs="--path $old_pwd/racr/larceny-bin"
		else
			libs="--path $old_pwd/racr/larceny-bin:$1"
		fi
		larceny --r6rs $libs --program $2
	fi
}

petite_run(){
	if [[ " ${selected_systems[@]} " =~ "petite" ]]
	then
		printf " Petite"
		if [ "$1" == "" ]
		then
			libs="--libdirs $old_pwd"
		else
			libs="--libdirs $old_pwd:$1"
		fi
		petite $libs --program $2
	fi
}

end_run(){
	echo ""
}

################################################################################################################## Execute tests:
echo "=========================================>>> Run Tests:"

# Test basic API:
cd $old_pwd/tests
for f in *.scm
do
	begin_run $f
	racket_run "" $f
	guile_run "" $f
	larceny_run "" $f
	petite_run "" $f
	end_run
done

# Test state-machines example:
cd $old_pwd/examples/state-machines
begin_run state-machines.scm
racket_run "" state-machines.scm
guile_run "" state-machines.scm
larceny_run "" state-machines.scm
petite_run "" state-machines.scm
end_run

# Test atomic petrinets example:
cd $old_pwd/examples/atomic-petrinets/examples
for f in *.scm
do
	begin_run $f
	racket_run "./../racket-bin" $f
	guile_run "./../guile-bin" $f
	larceny_run "./../larceny-bin" $f
	petite_run "./../.." $f
	end_run
done

# Test petrinets example:
cd $old_pwd/examples/petrinets/examples
for f in *.scm
do
	begin_run $f
	racket_run "./../racket-bin" $f
	# Disabled because of issue 37: guile_run "./../guile-bin" $f
	larceny_run "./../larceny-bin" $f
	petite_run "./../.." $f
	end_run
done

# Test SiPLE example:
cd $old_pwd/examples/siple/examples/correct
for f in *.siple
do
	begin_run $f
	racket_run "./../../racket-bin" "./../run-correct.scm $f"
	guile_run "./../../guile-bin" "./../run-correct.scm $f"
	larceny_run "./../../larceny-bin" "./../run-correct.scm -- $f"
	petite_run "./../../.." "./../run-correct.scm $f"
	end_run
done
cd $old_pwd/examples/siple/examples/incorrect
for f in *.siple
do
	begin_run $f
	racket_run "./../../racket-bin" "./../run-incorrect.scm $f"
	guile_run "./../../guile-bin" "./../run-incorrect.scm $f"
	larceny_run "./../../larceny-bin" "./../run-incorrect.scm -- $f"
	petite_run "./../../.." "./../run-incorrect.scm $f"
	end_run
done

# Test Tiny C++ example:
cd $old_pwd/profiling/tinycpp/examples
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "Tiny C++ Racket:"
	./run-examples.bash Racket
fi
if [[ " ${selected_systems[@]} " =~ "guile" ]]
then
	echo "Tiny C++ Guile:"
	./run-examples.bash Guile
fi
if [[ " ${selected_systems[@]} " =~ "larceny" ]]
then
	echo "Tiny C++ Larceny:"
	./run-examples.bash Larceny
fi
if [[ " ${selected_systems[@]} " =~ "petite" ]]
then
	echo "Tiny C++ Petite Chez Scheme:"
	./run-examples.bash Petite
fi

cd $old_pwd
