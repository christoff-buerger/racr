#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
call_dir=`pwd`

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/extract.bash" -h
	exit $?
fi
while getopts c:t:s:iaxh opt
do
	case $opt in
		c)
			if [ -z ${profiling_configuration+x} ]
			then
				profiling_configuration="$OPTARG"
			else
				echo " !!! ERROR: Several profiling configurations selected via -c flag !!!" >&2
				exit 2
			fi;;
		t)
			if [ -z ${measurements_table+x} ]
			then
				measurements_table="$OPTARG"
			else
				echo " !!! ERROR: Several measurement tables selected via -t flag !!!" >&2
				exit 2
			fi;;
		s)
			if [ -z ${rerun_script+x} ]
			then
				rerun_script="$OPTARG"
			else
				echo " !!! ERROR: Several rerun script names selected via -s flag !!!" >&2
				exit 2
			fi;;
		i|a|x)
			if [ -z ${recording_mode+x} ]
			then
				recording_mode="-$opt"
			else
				echo " !!! ERROR: Several recording modes selected via -i, -a or -x flag !!!" >&2
				exit 2
			fi;;
		h|?)
			echo "Usage: -c Profiling configuration (mandatory parameter)." >&2
			echo "       -t Measurements table used to record extracted measurements (mandatory parameter)." >&2
			echo "          Created if not existent." >&2
			echo "       -s Save rerun script (optional parameter)." >&2
			echo "          Can be used to redo the extraction." >&2
			echo "       -- List of source tables to extract from." >&2
			echo "          Must be non-empty." >&2
			echo "       At most one of the following recording modes (optional flags):" >&2
			echo "          -a Append extracted measurements on recording table." >&2
			echo "          -x Overwrite recording table with extracted measurements." >&2
			echo "          -i Integrate extracted measurements in recording table." >&2
			echo "             The recording table is treated as another source table and overwritten." >&2
			echo "       By default, extracted measurements are integrated." >&2
			echo "       Recording tables are NOT changed in case of errors." >&2
			echo "       " >&2
			echo "       Extraction operators (extract all rows whose columns satisfy their operators):" >&2
			echo "          - Wild-card: *" >&2
			echo "            No restriction on respective column values." >&2
			echo "          - Value comparator: <, >, <=, >=, ==, !=" >&2
			echo "            Compare column values with constant. Extract the ones satisfying the comparator." >&2
			echo "          - Row independet extrema: MIN, MAX" >&2
			echo "            Extract extremum of column." >&2
			echo "          - Row specific extrema: min, max" >&2
			echo "            Extract extremum of column for each row combination." >&2
			echo "       Row independent and specific extrema are non-exclusive." >&2
			echo "       The row of a cell being an extremum is selected if it satisfies all value comparators." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$@] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${rerun_script+x} ]
then
	rerun_script="/dev/null"
elif [ -z "$rerun_script" ] || [ ! "$rerun_script" -ef "/dev/null" -a -e "$rerun_script" ]
then
	echo " !!! ERROR: Invalid rerun script specified via -s flag !!!" >&2
	exit 2
fi

if [ -z ${recording_mode+x} ]
then
	recording_mode="-i"
fi

##################################################################################### Configure temporary and external resources:
measurements_date=`date "+%Y-%m-%d_%H-%M-%S"`
measurements_pipe="$script_dir/$measurements_date.measurements-pipe"
recording_table="$script_dir/$measurements_date.measurements-table"
valid_parameters=0

my_exit(){
	exit_status=$?
	if [ -t 0 ] && [ $exit_status -gt 0 ] && [ $valid_parameters -eq 0 ] && [ ! "$rerun_script" -ef "/dev/null" ]
	then # user specified invalid measurement-parameters while generating rerun script
		rm "$rerun_script"
	fi
	rm -f "$measurements_pipe"
	rm -f "$recording_table"
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 9 15

mkfifo "$measurements_pipe"

"$script_dir/record.bash" -c "$profiling_configuration" -t "$measurements_table" -p "$measurements_pipe" -x

for a in "$@"
do
	source_tables+=( "$a" )
	shift
done

if [ ! "$rerun_script" -ef "/dev/null" ]
then
	mkdir -p "`dirname "$rerun_script"`"
	touch "$rerun_script"
	chmod +x "$rerun_script"
fi
echo "#!/bin/bash" >> "$rerun_script"
echo "set -e" >> "$rerun_script"
echo "set -o pipefail" >> "$rerun_script"
echo "cd \"$call_dir\"" >> "$rerun_script"
echo "\"$script_dir/extract.bash\" \\" >> "$rerun_script"
echo "	-c \"$profiling_configuration\" \\" >> "$rerun_script"
echo "	-t \"$measurements_table\" \\" >> "$rerun_script"
echo "	-s /dev/null \\" >> "$rerun_script"
echo "	$recording_mode \\" >> "$rerun_script"
echo "-- ${source_tables[@]} << \|EOF\|" >> "$rerun_script"

if [ -f "$measurements_table" ]
then
	if [ "$recording_mode" == "-a" ]
	then
		cp "$measurements_table" "$recording_table"
	elif [ "$recording_mode" == "-i" ]
	then
		source_tables+=( "$measurements_table" )
	fi
fi

"$script_dir/record.bash" -c "$profiling_configuration" -t "$recording_table" -p "$measurements_pipe" &
sleep 1 # let the record script write the table header

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

################################################################################################################ Read parameters:
for (( i = 0; i < number_of_parameters; i++ ))
do
	read -r -p "${parameter_descriptions[$i]} [${parameter_names[$i]}]: " choice
	if [ ! -t 0 ]
	then
		echo "${parameter_descriptions[$i]} [${parameter_names[$i]}]: $choice"
	fi
	echo "$choice" >> "$rerun_script"
	extractors+=( "$choice" )
	
	case "$choice" in
		\<|\>|\<=|\>=|==|!=)
			read -r -p "	value $choice " choice2
			if [ ! -t 0 ]
			then
				echo "	value $choice $choice2"
			fi
			echo "$choice2" >> "$rerun_script"
			comparator_constants+=( "$choice2" )
			if [ -z "$choice2" -o ${#choice2[@]} -ne 1 ]
			then
				echo " !!! ERROR: Invalid choice !!!" >&2
				exit 2
			fi;;
		\*|MIN|MAX|min|max)
			comparator_constants+=( "" );;
		*)
			echo " !!! ERROR: Invalid choice !!!" >&2
			exit 2;;
	esac
done
echo "\|EOF\|" >> "$rerun_script"
valid_parameters=1

########################################################################################################### Extract measurements:

################################################################################# Finish execution & cleanup temporary resources:
cp "$recording_table" "$measurements_table"
my_exit
