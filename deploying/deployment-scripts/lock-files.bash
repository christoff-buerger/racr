#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if [ $# -eq 0 ]
then
	echo "Usage: -- List of files to lock (mandatory parameter)." >&2
	echo "          Any kind of files are supported: directories, named pipes, symlinks and regular files." >&2
	echo "          Only the directory of each file must exist; the file itself can be virtual." >&2
	echo "          All files are locked in a single atomic operation; if not, the script spins until they can." >&2
	echo "       " >&2
	echo "       As soon as successful, the absolute paths of the individual locks are returned (one for each file)." >&2
	echo "       Locks are released by deleting them (rm -f lock)." >&2
	echo "       Users are responsible to release locks; otherwise files stay locked forever." >&2
	echo "       There is no deadlock detection whatsoever." >&2
	echo "       " >&2
	echo "       This script supports soft-locking only:" >&2
	echo "          - To lock files, and respect locks, is voluntarily. Third parties not using the script to acquire" >&2
	echo "            beforehand locks on the files they want to operate on still can do so, even if these files are" >&2
	echo "            properly locked by somebody else." >&2
	echo "          - Only by systematic script application and thereby locking of potentially contested resources" >&2
	echo "            race conditions on such can be avoided." >&2
	echo "          - All RACR scripts partake; they are thread-safe and suited for parallel execution." >&2
	echo "       " >&2
	exit 2
elif [ "$1" != "--" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

#################################################################### Handle special case of no files (just exit => optimization):
if [ $# -eq 1 ]
then
	exit 0
fi
shift # --

################################################################## Check all files and derive absolute file paths and lock files:
declare -A lock_files
lock_files=()
for f in "$@"
do
	if [ ! -d "$( dirname "$f" )" ]
	then
		echo " !!! ERROR: Invalid file [$f]; the directory of the file does not exist !!!" >&2
		exit 2
	fi
	file_dir_absolute="$( cd "$( dirname "$f" )" && pwd )"
	file_normalized="$file_dir_absolute/$( basename "$f")"
	if [ -v "lock_files[$file_normalized]" ]
	then
		echo " !!! ERROR: Multiple locking of [$file_normalized] !!!" >&2
		exit 2
	fi
	lock_files["$file_normalized"]="$script_dir/file-locks$file_normalized"
	mkdir -p "$script_dir/file-locks$file_dir_absolute"
done

################################################ Handle special case of single file (no lock on script required => optimization):
if [ $# -eq 1 ]
then
	lock="${lock_files[*]}"
	while true
	do
		if ! ln "$script_dir/lock-files.bash" "$lock" > /dev/null 2>&1
		then
			sleep 1
		else
			echo "$lock"
			exit 0
		fi
	done
fi

#### Handle general case of many files (succeed only if ALL files can be locked => requires atomic operation to avoid deadlocks):
mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-files.bash" )"

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release lock:
	rm -f "$mutex"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

while true
do
	locked=()
	for f in "${lock_files[@]}"
	do
		if ! ln "$script_dir/lock-files.bash" "$f" > /dev/null 2>&1
		then
			break
		else
			locked+=( "$f" )
		fi
	done
	if (( ${#locked[@]} < ${#lock_files[@]} ))
	then
		for f in "${locked[@]}"
		do
			rm -f "$f"
		done
		sleep 5
	else
		break
	fi
done

for f in "${lock_files[@]}"
do
	echo "$f"
done

############################################################################################################## Cleanup resources:
exit 0 # triggers 'my_exit'
