#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
call_dir="$( pwd )"

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
				echo " !!! ERROR: Several profiling configurations selected via -c parameter !!!" >&2
				exit 2
			fi;;
		t)
			if [ -z ${measurements_table+x} ]
			then
				measurements_table="$OPTARG"
			else
				echo " !!! ERROR: Several measurement tables selected via -t parameter !!!" >&2
				exit 2
			fi;;
		s)
			if [ -z ${rerun_script+x} ]
			then
				rerun_script="$OPTARG"
			else
				echo " !!! ERROR: Several rerun script names selected via -s parameter !!!" >&2
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
			echo "       -- List of source tables to extract from (mandatory parameter)." >&2
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
			echo "            No further restrictions on respective column values." >&2
			echo "            Stops the querying of extraction operators for a column." >&2
			echo "          - Value comparator: <, >, <=, >=, ==, !=" >&2
			echo "            Compare column values with constant; extract the ones satisfying the comparator." >&2
			echo "            All value comparators must be satisfied for a row to be extracted." >&2
			echo "          - Row independent extrema: MIN, MAX" >&2
			echo "            Extract extremum of column." >&2
			echo "          - Row specific extrema: min, max" >&2
			echo "            Extract extremum of column for each row combination." >&2
			echo "       " >&2
			echo "       The row of a cell being an extremum is selected if it satisfies all value comparators." >&2
			echo "       Each column can have several extraction operators:" >&2
			echo "          - Value comparators are subtractive (logical conjunction: and)." >&2
			echo "            Column cells must satisfy all their comparators." >&2
			echo "          - Extrema are additive (logical disjunction: or)." >&2
			echo "            It is sufficient if a column cell satisfies one of its extrema." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [ -z "$measurements_table" ] || { [ -e "$measurements_table" ] && [ ! -f "$measurements_table" ]; }
then
	echo " !!! ERROR: Invalid or no measurements table specified via -t parameter !!!" >&2
	exit 2
fi

if [ -z ${rerun_script+x} ]
then
	rerun_script="/dev/null"
elif [ -z "$rerun_script" ] || { [ ! "$rerun_script" -ef "/dev/null" ] && [ -e "$rerun_script" ]; }
then
	echo " !!! ERROR: Invalid rerun script specified via -s parameter !!!" >&2
	exit 2
fi

for a in "$@"
do
	if [ ! -f "$a" ]
	then
		echo " !!! ERROR: Non-existing source table [$a] specified via '--' argument list !!!" >&2
		exit 2
	fi
	source_tables+=( "$a" )
	shift
done
if [ -z ${source_tables+x} ]
then
	echo " !!! ERROR: No source table specified via '--' argument list !!!" >&2
	exit 2
fi

if [ -z ${recording_mode+x} ]
then
	recording_mode="-i"
fi

##################################################################################### Configure temporary and external resources:
tmp_dir=""
unset recording_pid
valid_parameters=0
extraction_successful=0

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Close the recording pipe and wait until all extracted measurements are recorded:
	if [ -n "${recording_pid+x}" ]
	then
		exec 3>&-
		while s=$( ps -p "$recording_pid" -o state= ) && [[ "$s" && "$s" != 'Z' ]] 
		do
			sleep 1
		done
	fi
	# Update the final measurements table if, and only if, everything was fine:
	if [ $extraction_successful -eq 1 ]
	then
		mkdir -p "$( dirname "$measurements_table" )"
		mv -f "$extraction_table" "$measurements_table"
	fi
	# Delete the rerun script in case a user interactively specified invalid extraction-parameters while generating it:
	if [ -t 0 ] && [ $exit_status -gt 0 ] && [ $valid_parameters -eq 0 ] && [ ! "$rerun_script" -ef "/dev/null" ]
	then
		rm "$rerun_script"
	fi
	# Delete all temporary resources:
	rm -rf "$tmp_dir"
	# Return captured exit status (i.e., if the original script execution succeeded or not):	
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

tmp_dir="$( "$script_dir/../../deploying/deployment-scripts/create-temporary.bash" -t d )"
extraction_pipe="$tmp_dir/extraction-pipe.fifo"
extraction_table="$tmp_dir/extraction-table.txt"
extraction_script="$tmp_dir/extraction-script.scm"

if [ -f "$measurements_table" ]
then
	if [ "$recording_mode" == "-a" ]
	then
		cp -p "$measurements_table" "$extraction_table"
	elif [ "$recording_mode" == "-i" ]
	then
		source_tables+=( "$measurements_table" )
	else
		# Extreme sanity check to avoid accidental overwriting of arbitrary files:
		"$script_dir/check-tables.bash" -c "$profiling_configuration" -- "$measurements_table"
	fi
fi

mkfifo "$extraction_pipe"
recording_pid=$( "$script_dir/record.bash" -c "$profiling_configuration" -t "$extraction_table" -p "$extraction_pipe" )
exec 3> "$extraction_pipe"

mapfile -t installed_systems < <( "$script_dir/../../deploying/deployment-scripts/list-scheme-systems.bash" -i || kill -13 $$ )
selected_system="${installed_systems[0]}"

if [ ! "$rerun_script" -ef "/dev/null" ]
then
	mkdir -p "$( dirname "$rerun_script" )"
	touch "$rerun_script"
	chmod +x "$rerun_script"
fi
source_tables_string=""
for s in "${source_tables[@]}"
do
	source_tables_string="$source_tables_string \"$s\""
done
{
echo "#!/usr/bin/env bash"
echo "set -e"
echo "set -o pipefail"
echo "if [ ! \$# -eq 0 ]"
echo "then"
echo "	echo \" !!! ERROR: Unknown [\$*] command line arguments !!!\" >&2"
echo "	exit 2"
echo "fi"
echo "cd \"$call_dir\""
echo "\"$script_dir/extract.bash\" \\"
echo "	-c \"$profiling_configuration\" \\"
echo "	-t \"$measurements_table\" \\"
echo "	-s /dev/null \\"
echo "	$recording_mode \\"
echo "--$source_tables_string << \"\|EOF\|\""
} > "$rerun_script"

############################################################################################################# Read configuration:
. "$script_dir/configure.bash" # Sourced script sets configuration!

####################################################################################################### Read extraction criteria:
extractors=" (list"
j=-1
for (( i = 0; i < number_of_criteria; i++ ))
do
	if (( j < i ))
	then
		echo "${criteria_descriptions[$i]} [${criteria_names[$i]}]:"
		j=$(( j + 1 ))
		extractors="$extractors"$'\n'"  (list"
	fi
	IFS='' read -r -p "        Extraction operator: " choice
	if [ ! -t 0 ]
	then
		echo "        Extraction operator: $choice"
	fi
	printf "%s\n" "$choice" >> "$rerun_script" # Save as read!
	
	case "$choice" in
		\<|\>|\<=|\>=|==|!=)
			extractors="$extractors ps:$choice"
			echo -en "\033[1A\033[$((30 + ${#choice}))C"
			IFS='' read -r choice2
			if [ ! -t 0 ]
			then
				echo "$choice2"
			fi
			printf "%s\n" "$choice2" >> "$rerun_script" # Save as read!
			if [ -z "$choice2" ] || [ ${#choice2[@]} -ne 1 ]
			then
				echo " !!! ERROR: Invalid choice !!!" >&2
				exit 2
			fi
			extractors="$extractors \"$choice2\""
			i=$(( i - 1 ));;
		min|max|MIN|MAX)
			extractors="$extractors ps:$choice"
			i=$(( i - 1 ));;
		\*)
			extractors="$extractors)";;
		*)
			echo " !!! ERROR: Invalid choice !!!" >&2
			exit 2;;
	esac
done
echo "\|EOF\|" >> "$rerun_script"
valid_parameters=1
extractors="$extractors)"

##################################################################################################### Generate extraction script:
{
echo "#!r6rs"
echo ""
echo "(import (rnrs) (profiling-scripts extract))"
echo ""
echo "(filter-tables"
printf " (list"
for s in "${criteria_names[@]}"
do
	printf "\n  \"%s\"" "$s"
done
echo ")"
printf "%s" "$extractors"
for s in "${source_tables[@]}"
do
	printf "\n \"%s\"" "$s"
done
echo ")"
} > "$extraction_script"

chmod +x "$extraction_script"

########################################################################################################### Extract measurements:
"$script_dir/../../deploying/deployment-scripts/execute.bash" -s "$selected_system" -l "$script_dir" -e "$extraction_script" >&3
extraction_successful=1

############################################################## Update the final measurements table & cleanup temporary resources:
exit 0 # triggers 'my_exit'
