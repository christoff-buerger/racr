#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
arguments="$* --"
arguments="${arguments#*--}"

if [ $# -eq 0 ]
then
	"$script_dir/lock-files.bash" -h
	exit $?
fi

while getopts x:k:h opt
do
	case $opt in
		x)
			if [[ ! -v "no_spinning" ]]
			then
				no_spinning="$OPTARG"
			else
				echo " !!! ERROR: Several error messages selected via -x parameter !!!" >&2
				exit 2
			fi
			;;
		k)
			if [[ ! -v "reentrance_key" ]]
			then
				reentrance_key="$OPTARG"
			else
				echo " !!! ERROR: Several re-entrance keys selected via -k parameter !!!" >&2
				exit 2
			fi
			;;
		h|?)
			echo "Usage: -x Error message if locks can not be acquired on first attempt (optional parameter)." >&2
			echo "          Can be used to avoid spinning." >&2
			echo "       -k Re-entrance key for key-protected files (optional parameter)." >&2
			echo "          For files locked the first time, the given key is set." >&2
			echo "          If files are already locked without key, '-k' spins; with '-x' it fails." >&2
			echo "          Further re-entrances must provide the key or locking spins; with '-x' they fail." >&2
			echo "          Re-entrances with a correct key for key-protected files succeeds without spinning." >&2
			echo "          Locking attempts without '-k' spin for key-protected files; with '-x' they fail." >&2
			echo "       -- List of files to lock (mandatory parameter)." >&2
			echo "          The supported file types are: named pipes, symlinks and regular files." >&2
			echo "          Locking directories is not supported." >&2
			echo "          Only the parent directory of each file must exist; the file itself can be virtual." >&2
			echo "          All files are locked in a single atomic operation; if not, the script spins until" >&2
			echo "          they can, except if the '-x' parameter is given in which case the script fails" >&2
			echo "          with the given error message." >&2
			echo "       " >&2
			echo "       For each file locked, the absolute path of a release-script is returned." >&2
			echo "       Release-script paths are returned in the order of the list of files to lock;" >&2
			echo "       Each release-script unlocks its respective file, or in case of a re-entrance" >&2
			echo "       reduces the re-entrance level by one whereas the file is finally unlocked when" >&2
			echo "       the re-entrance level of its lock becomes 0." >&2
			echo "       Users are responsible to use returned release-scripts to unlock files when done," >&2
			echo "       otherwise files stay locked forever." >&2
			echo "       There is no deadlock detection whatsoever." >&2
			echo "       " >&2
			echo "       This script supports soft-locking only:" >&2
			echo "          - To lock files, and respect locks, is voluntarily. Third parties not using the" >&2
			echo "            script to acquire beforehand locks on the files they want to operate on still can" >&2
			echo "            do so, even if these files are properly locked by somebody else." >&2
			echo "          - Only by systematic script application and thereby locking of potentially contested" >&2
			echo "            resources race conditions on such can be avoided." >&2
			echo "          - All RACR scripts partake; they are thread-safe and suited for parallel execution." >&2
			echo "       " >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ $# -ge 1 ] && [ " $* --" != "$arguments" ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

#################################################################### Handle special case of no files (just exit => optimization):
if [ $# -eq 0 ]
then
	exit 0
fi

################################################################## Check all files and derive absolute file paths and lock files:
declare -A files_set
# shellcheck disable=SC2034
file_set=() # We need to ensure the given files are a set, not a multi-set; but we also...
files=() # ...need to preserve order to return the paths of their release-scripts in order.
for f in "$@"
do
	if [ -d "$f" ]
	then
		echo " !!! ERROR: Invalid file [$f]; locking directories is not supported !!!" >&2
		exit 2
	fi
	f_dirname="$( dirname "$f" )"
	if [ ! -d "$f_dirname" ]
	then
		echo " !!! ERROR: Invalid file [$f]; the parent directory of the file does not exist !!!" >&2
		exit 2
	fi
	file_dir_absolute="$( cd "$f_dirname" && pwd )"
	file_normalized="$file_dir_absolute/$( basename "$f" )"
	if [[ -v "files_set[$file_normalized]" ]]
	then
		echo " !!! ERROR: Multiple locking of [$file_normalized] !!!" >&2
		exit 2
	fi
	# shellcheck disable=SC2034
	files_set["$file_normalized"]="_"
	files+=( "$file_normalized" )
	mkdir -p "$script_dir/file-locks/atomic-locks$file_dir_absolute"
	mkdir -p "$script_dir/file-locks/keys$file_dir_absolute"
	mkdir -p "$script_dir/file-locks/release-scripts$file_normalized"
done

########################### Handle special case of single, no re-entrance file lock (no lock on script required => optimization):
if [ $# -eq 1 ] && [[ ! -v "reentrance_key" ]]
then
	f="${files[*]}"
	atomic_lock="$script_dir/file-locks/atomic-locks$f"
	release_script="$script_dir/file-locks/release-scripts$f/release.bash"
	while true
	do
		if ln "$script_dir/lock-files.bash" "$atomic_lock" > /dev/null 2>&1
		then
			{
			printf "#!/usr/bin/env bash\n"
			printf "rm -f \"%s\"\n" "$release_script"
			printf "rm -f \"%s\"" "$atomic_lock"
			} > "$release_script"
			chmod +x "$release_script"
			echo "$release_script"
			exit 0
		else
			if [[ -v "no_spinning" ]]
			then
				echo "$no_spinning" >&2
				exit 2
			fi
			sleep 1
		fi
	done
fi

############################################# Handle general case of many files or re-entrance locks (requires atomic operation):
my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release locks:
	if (( not_suspended == 1 ))
	then
		"$mutex"
	fi
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}

# Mutex is shared with release-scripts for key-protected files to avoid race conditions with parallel unlocking:
trap 'my_exit' 0 1 2 3 15
mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-files.bash" )"
not_suspended=1

while true
do
	succeeded=0
	new_atomic_locks=()
	for f in "${files[@]}"
	do
		atomic_lock="$script_dir/file-locks/atomic-locks$f"
		key="$script_dir/file-locks/keys$f"
		if ln "$script_dir/lock-files.bash" "$atomic_lock" > /dev/null 2>&1
		then
			succeeded=$(( succeeded + 1 ))
			new_atomic_locks+=( "$atomic_lock" )
		elif [[ -v "reentrance_key" ]] && [ -f "$key" ] && [ "$reentrance_key" == "$( cat "$key" )" ]
		then
			succeeded=$(( succeeded + 1 ))
		else
			break
		fi
	done
	if (( succeeded < ${#files[@]} ))
	then
		for atomic_lock in "${new_atomic_locks[@]}"
		do
			rm -f "$atomic_lock"
		done
		if [[ -v "no_spinning" ]]
		then
			echo "$no_spinning" >&2
			exit 2
		fi
		"$mutex" # Enable release-scripts to unlock contested files and parallel 'file-lock.bash' instances...
		not_suspended=0 # ...while suspended...
		sleep 5
		mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-files.bash" )" # ...but not while locking.
		not_suspended=1
	else
		break
	fi
done

for f in "${files[@]}"
do
	atomic_lock="$script_dir/file-locks/atomic-locks$f"
	key="$script_dir/file-locks/keys$f"
	if [[ -v "reentrance_key" ]]
	then
		release_script_dir="$script_dir/file-locks/release-scripts$f"
		release_script="$release_script_dir/release-$$-$((RANDOM))-$((RANDOM)).bash"
		while [ -e "$release_script" ]
		do
			release_script="$release_script_dir/release-$$-$((RANDOM))-$((RANDOM)).bash"
		done
		printf "%s" "$reentrance_key" > "$key"
		{
		printf "#!/usr/bin/env bash\n"
		# Mutex is shared with 'lock-files.bash' to avoid race conditions with parallel locking:
		printf "my_exit(){\n"
		printf "	\"\$mutex\"\n"
		printf "}\n"
		printf "mutex=\"\$( \"%s/lock-files.bash\" -- \"%s/lock-files.bash\" )\"\n" "$script_dir" "$script_dir"
		printf "trap 'my_exit' 0 1 2 3 15\n"
		printf "rm -f \"%s\"\n" "$release_script"
		printf "if find \"%s\" -maxdepth 0 -empty | read\n" "$release_script_dir"
		printf "then\n"
		printf "	rm -f \"%s\"\n" "$key"
		printf "	rm -f \"%s\"\n" "$atomic_lock"
		printf "fi"
		} > "$release_script"
	else
		release_script="$script_dir/file-locks/release-scripts$f/release.bash"
		{
		printf "#!/usr/bin/env bash\n"
		printf "rm -f \"%s\"\n" "$release_script"
		printf "rm -f \"%s\"" "$atomic_lock"
		} > "$release_script"
	fi
	chmod +x "$release_script"
	echo "$release_script"
done

############################################################################################################## Cleanup resources:
exit 0 # triggers 'my_exit'
