#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

# Persist configuration in a script such that the measurements can be rerun:
echo "cd ../.." > rerun-measurements.bash
echo "./make-measurements.bash << EOF" >> rerun-measurements.bash

# Function always called when the script terminates:
my_exit(){
	# Ensure, the temporary rerun script is always deleted:
	cd $old_pwd
	rm rerun-measurements.bash
	exit 0
}
trap 'my_exit' 1 2 3 9 15

check_parameter(){
	read -r -p "$1" choice # No message printed on terminal if non-interactive. Thus,...
	if [ ! -t 0 ] # ...check if stdin is terminal? (test -t 0 == tty -s). If not...
	then
		echo "$1$choice" # ...print the read input.
	fi
	if [[ ! $choice =~ ^[1-9][0-9]*$ ]]
	then
		echo "ERROR: No integer number > 0 entered!"
		my_exit
	else
		eval $2=$choice
	fi
	echo $choice >> $old_pwd/rerun-measurements.bash
}

check_decision(){
	read -r -n1 -p "$1" choice
	if [ ! -t 0 ]
	then
		printf "$1$choice"
	fi
	echo ""
	case $choice in
		[y]* ) eval $2=$choice;;
		[n]* ) :;;
		* ) echo "ERROR: No valid choice entered!"; my_exit;;
	esac
	printf $choice >> $old_pwd/rerun-measurements.bash
}

declare -a systems=( Larceny Petite Racket Java_d Java_i Java_ic )

# Query & check measurement parameters:
check_parameter "Max. execution time (s) : " max_time
check_parameter "Min. global classes     : " min_classes
check_parameter "Max. global classes     : " max_classes
if (( min_classes > max_classes ))
then
	echo "   !!! ABORTED: Min. global classes > max. global classes !!!"
	my_exit
elif (( max_classes - min_classes > 1 ))
then
	check_parameter "Inc. global classes     : " inc_classes
	if (( (max_classes - min_classes) % inc_classes ))
	then
		echo "   !!! ABORTED: Difference of max. & min. global classes not multiple of increase !!!"
		my_exit
	fi
else
	inc_classes=1
fi
check_parameter "Min. class depth        : " min_depth
check_parameter "Max. class depth        : " max_depth
if (( min_depth > max_depth ))
then
	echo "   !!! ABORTED: Min. class depth > max. class depth !!!"
	my_exit
elif (( max_depth - min_depth > 1 ))
then
	check_parameter "Inc. class depth        : " inc_depth
	if (( (max_depth - min_depth) % inc_depth ))
	then
		echo "   !!! ABORTED: Difference of max. & min. class depth not multiple of increase !!!"
		my_exit
	fi
else
	inc_depth=1
fi
for system in ${systems[@]}
do
	check_decision "$system			: " $system
done
echo ""

# Create directories:
mkdir -p $old_pwd/measurements/programs
measurement_dir=$old_pwd/measurements/`date "+%Y-%m-%d_%H-%M-%S"`
mkdir -p $measurement_dir

# Finish & copy the rerun script:
echo "" >> rerun-measurements.bash
echo "EOF" >> rerun-measurements.bash
chmod +x rerun-measurements.bash
cp -p rerun-measurements.bash $measurement_dir

# Generate program:
cd $old_pwd/measurements/programs
for (( c = min_classes; c <= max_classes; c = c + inc_classes ))
do
	for (( d = min_depth; d <= max_depth; d = d + inc_depth ))
	do
		program=c$c-d$d.cpp
		if [ ! -f $program ]
		then
			printf "" > $program
			for (( c2 = 1; c2 <= c; c2++ ))
			do
				echo "class C$c2" >> $program
				echo "{" >> $program
				echo "public:" >> $program
				echo "	static int a;" >> $program
				echo "	class I1;" >> $program
				echo "	static void m(int i)" >> $program
				echo "	{" >> $program
				echo "		a = i;" >> $program
				if (( c2 > 1 ))
				then
					echo "		C"$(( c2 - 1 ))"::a = i;" >> $program
				fi
				echo "	}" >> $program
				echo "};" >> $program
				echo "" >> $program
				for (( d2 = 1; d2 < d; d2++ ))
				do
					printf "class C$c2" >> $program
					for (( i = 1; i <= d2; i++ ))
					do
						printf "::I$i" >> $program
					done
					echo "" >> $program
					echo "{" >> $program
					echo "public:" >> $program
					echo "	static int a;" >> $program
					echo "	class I"$(( d2 + 1 ))";" >> $program
					echo "	static void m(int i)" >> $program
					echo "	{" >> $program
					echo "		a = i;" >> $program
					for (( i = d2 - 1; i > 0; i-- ))
					do
						echo "		I$i::a = i;" >> $program
					done
					echo "		C$c2::a = i;" >> $program
					if (( c2 > 1 ))
					then
						printf "		C%i" $(( c2 - 1 )) >> $program
						for (( i = 1; i <= d2; i++ ))
						do
							printf "::I$i" >> $program
						done
						echo "::a = i;" >> $program
					fi
					echo "	}" >> $program
					echo "};" >> $program
					echo "" >> $program
				done	
			done
			echo "" >> $program
			echo "int main()" >> $program
			echo "{" >> $program
			echo "}" >> $program
		fi
	done
done

# Perform measurements:

measure(){
	if [ "`echo ${aborted[@]} | grep $system`" != "" ]
	then
		echo " ABORTED"
		rm $res_program
		return
	fi
	error_file=$res_program-errors.txt
	start=`date +"%s"`
	$* &> $error_file
	elapsed=$(( `date +"%s"` - start ))
	if [ -s $error_file ]
	then
		echo " PROGRAM ERROR"
	else
		if [ ! -f $src_program-result ]
		then
			cp -p $res_program $src_program-result
			printf " New reference result in"
		else
			diff_result=`diff --text $res_program $src_program-result`
			if [ "$diff_result" != "" ]
			then
				echo " RESULT ERROR"
				echo "$diff_result" >> $error_file
				return
			fi
		fi
		rm $error_file
		if [ ! -e $measurement_dir/measurements.table ]
		then
			echo "  System  | Classes  |  Depth   | Rewrites |   Time   " > $measurement_dir/measurements.table
			echo "----------+----------+----------+----------+----------" >> $measurement_dir/measurements.table
		fi
		printf " %-8s | %8i | %8i | %8i | %8i \n" \
			$system $c $d $(( c * (d - 1) )) $elapsed >> $measurement_dir/measurements.table
		echo " ${elapsed}s"
		if (( $elapsed > max_time ))
		then
			aborted+=( $system )
		fi
	fi
	rm $res_program
}

for (( c = min_classes; c <= max_classes; c = c + inc_classes ))
do
	aborted=( )
	for (( d = min_depth; d <= max_depth; d = d + inc_depth ))
	do
		echo "Classes/Depth: $c, $d"
		for system in ${systems[@]}
		do
			if [ -n "${!system}" ]
			then
				printf "	$system		:"
				src_program=$old_pwd/measurements/programs/c$c-d$d.cpp
				res_program=$measurement_dir/$system-c$c-d$d.cpp
				cp -p $src_program $res_program
				case $system in
				Larceny)
					measure larceny --r6rs --path "$old_pwd/tinycpp-racr/larceny-bin:$old_pwd/../../racr/larceny-bin" \
						--program $old_pwd/compile-correct.scm -- $res_program;;
				Petite)
					measure petite --libdirs "$old_pwd:$old_pwd/../.." \
						--program $old_pwd/compile-correct.scm $res_program;;
				Racket)
					measure plt-r6rs ++path "$old_pwd/tinycpp-racr/racket-bin" ++path "$old_pwd/../../racr/racket-bin" \
						$old_pwd/compile-correct.scm $res_program;;
				Java_d)
					measure java -jar $old_pwd/tinycpp-jastadd/tinycpp-declarative.jar $res_program;;
				Java_i)
					measure java -jar $old_pwd/tinycpp-jastadd/tinycpp-iterative.jar $res_program;;
				Java_ic)
					measure java -jar $old_pwd/tinycpp-jastadd/tinycpp-iterative-cached.jar $res_program;;
				*) # Never encountered!
					echo "++++++++++++++++++++++++++++++++++++ SCRIPT-ERROR ++++++++++++++++++++++++++++++++++++";;
				esac
			fi
		done
	done
	echo ""
done

my_exit
