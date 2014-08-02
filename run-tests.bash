#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

begin_run(){
	echo `pwd`/$1
}

racket_run(){
	if which plt-r6rs > /dev/null
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

larceny_run(){
	if which larceny > /dev/null
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
	if which petite > /dev/null
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

echo "=========================================>>> Run Tests:"

# Test basic API:
cd $old_pwd/tests
for f in *.scm
do
	begin_run $f
	racket_run "" $f
	larceny_run "" $f
	petite_run "" $f
	end_run
done

# Test state-machine example:
cd $old_pwd/examples/state-machines
begin_run state-machines.scm
racket_run "" state-machines.scm
larceny_run "" state-machines.scm
petite_run "" state-machines.scm
end_run

# Test petrinets example:
cd $old_pwd/examples/petrinets/examples
for f in *.scm
do
	begin_run $f
	racket_run "./../racket-bin" $f
	larceny_run "./../larceny-bin" $f
	petite_run "./../.." $f
	end_run
done

# Test siple example:
cd $old_pwd/examples/siple/examples/correct
for f in *.siple
do
	begin_run $f
	racket_run "./../../racket-bin" "./../run-correct.scm $f"
	larceny_run "./../../larceny-bin" "./../run-correct.scm -- $f"
	petite_run "./../../.." "./../run-correct.scm $f"
	end_run
done
cd $old_pwd/examples/siple/examples/incorrect
for f in *.siple
do
	begin_run $f
	racket_run "./../../racket-bin" "./../run-incorrect.scm $f"
	larceny_run "./../../larceny-bin" "./../run-incorrect.scm -- $f"
	petite_run "./../../.." "./../run-incorrect.scm $f"
	end_run
done

cd $old_pwd
