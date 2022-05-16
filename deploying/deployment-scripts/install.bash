#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

selected_systems_array=()
selected_libraries_array=()
unset skip_tests

################################################################################################################ Parse arguments:
while getopts s:l:i:xh opt
do
	case $opt in
		s)
			if [[ ! -v "skip_tests" ]]
			then
				"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			fi
			selected_systems_array+=( "$OPTARG" )
			;;
		l)
			mapfile -O ${#selected_libraries_array[@]} -t selected_libraries_array < <(
				"$script_dir/list-libraries.bash" -l "$OPTARG" \
				|| kill -13 $$ )
			;;
		i)
			if [[ -v "skip_tests" ]]
			then
				selected_libraries_array+=( "$OPTARG" )
			else			
				library_configuration="$( "$script_dir/list-libraries.bash" -c "$OPTARG" )"
				mapfile -O ${#selected_libraries_array[@]} -t selected_libraries_array < <(
					dirname "$library_configuration" \
					|| kill -13 $$ )
			fi
			;;
		x)
			skip_tests="true"
			;;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter)." >&2
			echo "          Permitted values:" >&2
			"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /' >&2
			echo "          If no Scheme system is selected, the selected RACR libraries" >&2
			echo "          are installed for all available systems." >&2
			echo "       -l RACR library to install (optional multi-parameter)." >&2
			echo "          Permitted values:" >&2
			"$script_dir/list-libraries.bash" -k | sed 's/^/             /' >&2
			echo "       -i RACR library directory of library to install (optional multi-parameter)." >&2
			echo "          Permitted values:" >&2
			"$script_dir/list-libraries.bash" -i | sed 's/^/             /' >&2
			echo "       -x Skip tests (internal multi-flag)." >&2
			echo "          Do not test any of the following '-s' and '-i' options." >&2
			echo "          Used to avoid superfluous double checks when recurring." >&2
			echo "          Must never be called by users, i.e., non internal, non recursive calls." >&2
			echo "" >&2
			echo "       If no library is selected via '-l' or '-i', all libraries are installed." >&2
			exit 64
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

if [[ ! -v "selected_systems_array[@]" ]]
then
	mapfile -t selected_systems_array < <( "$script_dir/list-scheme-systems.bash" -i || kill -13 $$ )
fi
declare -A selected_systems # Use associative array as set, to avoid double entries.
selected_systems=()
for s in "${selected_systems_array[@]}"
do
	selected_systems["$s"]="$s"
done

if [[ ! -v "selected_libraries_array[@]" ]]
then
	mapfile -t selected_libraries_array < <( "$script_dir/list-libraries.bash" -i || kill -13 $$ )
fi
declare -A selected_libraries # Use associative array as set, to avoid double entries.
selected_libraries=()
for l in "${selected_libraries_array[@]}"
do
	selected_libraries["$l"]="$l"
done

############################################################################################################ Configure resources:
my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Wait for installation co-routines to cleanup first:
	wait
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

##################################################################### Define installation procedures for specific Scheme systems:
install_chez(){
	installation_directory="$binaries/$( basename "$library" )"
	mkdir -p "$installation_directory"
	library_paths_string="$binaries"
	for l in "${required_libraries[@]}"
	do
		library_paths_string+=":$l/binaries/$system"
	done
	for s in "${required_sources[@]}"
	do
		s_basename="$( basename "$s" )"
		chez --libdirs "$library_paths_string" -q --optimize-level 3 << \
EOF
		(compile-library "$s.scm" "$installation_directory/$s_basename.so")
EOF
	done
}

install_guile(){
	installation_directory="$binaries/$( basename "$library" )"
	mkdir -p "$installation_directory"
	library_paths=( --load-path="$binaries" )
	for l in "${required_libraries[@]}"
	do
		library_paths+=( --load-path="$l/binaries/$system" )
	done
	for s in "${required_sources[@]}"
	do
		s_basename="$( basename "$s" )"
		cp -p "$s.scm" "$installation_directory"
		# shellcheck disable=SC2034 # 'GUILE_AUTO_COMPILE' only used indirectly by 'guild'.
		GUILE_AUTO_COMPILE=0 # Workaround for broken '--no-auto-compile' flag.
		guild \
			compile \
			--optimize=3 \
			"${library_paths[@]}" \
			--output="$installation_directory/$s_basename.go" \
			"$installation_directory/$s_basename.scm"
	done
}

install_racket(){
	#installation_directory="$binaries/$( basename "$library" )"
	#mkdir -p "$installation_directory"	
	library_paths=( ++path "$binaries" )
	for l in "${required_libraries[@]}"
	do
		library_paths+=( ++path "$l/binaries/$system" )
	done
	for s in "${required_sources[@]}"
	do
		plt-r6rs "${library_paths[@]}" --install --collections "$binaries" "$s.scm"
	done
}

install_larceny(){
	installation_directory="$binaries/$( basename "$library" )"
	mkdir -p "$installation_directory"
	library_paths_string="$binaries"
	for l in "${required_libraries[@]}"
	do
		library_paths_string+=":$l/binaries/$system"
	done
	for s in "${required_sources[@]}"
	do
		s_basename="$( basename "$s" )"		
		cp -p "$s.scm" "$installation_directory/$s_basename.sls"
		larceny --utf8 --r6rs --path "$library_paths_string" << \
EOF
		(import (rnrs) (larceny compiler))
		(compiler-switches (quote fast-safe)) ; optimisation (even more aggressive: fast-unsafe)
		(compile-library "$installation_directory/$s_basename.sls")
EOF
	done
}

install_ironscheme(){
	library_basename="$( basename "$library" )"
	installation_directory="$binaries/$library_basename"
	mkdir -p "$installation_directory"	
	library_paths=( -I "$binaries" )
	for l in "${required_libraries[@]}"
	do
		library_paths+=( -I "$l/binaries/$system" )
	done
	compile_script="(import"
	for s in "${required_sources[@]}"
	do
		s_basename="$( basename "$s" )"
		cp -p "$s.scm" "$installation_directory/$s_basename.sls"
		compile_script="$compile_script ($library_basename $s_basename)"
	done
	echo "$compile_script)" > "$binaries/compile-script.sls"
	if [ "$library_basename" == "racr" ] # Adapt (racr core) and copy IronScheme.dll.
	then
		mv "$installation_directory/core.sls" "$installation_directory/core.scm"
		"$script_dir/../../racr-net/transcribe-racr-core.bash" "$installation_directory"
		rm "$installation_directory/core.scm"
		cp -p "$( dirname "$( command -v IronScheme.Console-v4.exe )" )/IronScheme.dll" "$binaries"
	fi
	# Use subshell for local directory changes via cd:
	(
	cd "$binaries"
	echo "(compile \"$binaries/compile-script.sls\")" | \
		mono "$( command -v IronScheme.Console-v4.exe )" -nologo "${library_paths[@]}"
	)
	rm -rf "$installation_directory" # Force usage of compiled IronScheme dll assemblies.
	rm "$binaries/compile-script.sls"
}

install_sagittarius(){
	installation_directory="$binaries/$( basename "$library" )"
	mkdir -p "$installation_directory"
	for s in "${required_sources[@]}"
	do
		cp -p -v "$s.scm" "$installation_directory"
	done
}

install_ypsilon(){
	installation_directory="$binaries/$( basename "$library" )"
	mkdir -p "$installation_directory"
	for s in "${required_sources[@]}"
	do
		cp -p -v "$s.scm" "$installation_directory"
	done
}

######################### Define encapsulated common procedures to concurrently install RACR libraries on varying Scheme systems:
install_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Release lock:
	"$1"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}

max_processes=$( ulimit -u )
safe_fork(){
	while (( $( ps -e | wc -l ) + 100 >= max_processes ))
	do
		sleep 1
	done
	"$@" &
	install_pids+=( $! )
}

safe_join(){
	joined_exit_status=0
	for p in "${install_pids[@]}"
	do
		set +e # Temporarily disable "fail on error".
		set +o pipefail
		wait "$p"
		exit_status=$?
		set -e # Restore "fail on error".
		set -o pipefail
		if (( exit_status != 0 ))
		then
			joined_exit_status=1
		fi
	done
	wait # Defensive programming (all co-routines should have finished anyway).
}

install_system()( # Encapsulated common procedure for Scheme system installation:
	# Configure the directory for the Scheme system specific binaries:
	system="$1"
	if [[ ! -v "supported_systems[$system]" ]]
	then
		exit 0
	fi
	binaries="$library/binaries/$system"
	mkdir -p "$binaries"
	
	if (( ${#required_libraries[@]} == 1 ))
	then # Avoid unnecessary spawning of background processes:
		set +e # Temporarily disable "fail on error".
		set +o pipefail	
		"$script_dir/install.bash" -x -s "$system" -i "${required_libraries[@]}"
		joined_exit_status=$(( $? != 0 ))
		set -e # Restore "fail on error".
		set -o pipefail
	else # Run an installation co-routine for each required library:
		install_pids=()
		for l in "${required_libraries[@]}"
		do
			safe_fork "$script_dir/install.bash" -x -s "$system" -i "$l"
		done
		safe_join
	fi
	
	# Combine hash of sources with hashes of required libraries to final hash capturing any kind of change:
	installation_hash="$(
		cat <( echo "$installation_hash" ) "${required_libraries[@]/%//binaries/$system/installation-hash.txt}" |
		openssl dgst -binary -sha3-512 |
		xxd -p -c 512 )"
	
	# Acquire lock for race-condition-free check if (re)installation is required and perform ALL changes:
	mutex="$( "$script_dir/lock-files.bash" -- "$binaries/lock" )"
	trap 'install_exit "$mutex"' 0 1 2 3 15
	
	# Delete existing installation in case required libraries failed to install and write new hash:
	if (( joined_exit_status == 1 ))
	then
		rm -rf "${binaries:?}/"*
		echo "$installation_hash" > "$binaries/installation-hash.txt"
		touch "$binaries/installation-failed"
		echo " !!! ERROR: A required RACR library failed to install !!!" > "$binaries/install-log.txt"
		log="$( < "$binaries/install-log.txt" )"
		# Atomic stdout:
		printf "\n\033[0;31m===>>> Install [%s] for [%s]:\033[0m\n\n%s\n" \
			"$library" \
			"$system" \
			"$log" \
			>&2
		exit 1
	fi
	
	# Avoid reinstallation if existing installation is up-to-date (requires LOCKED reading of hash):
	if [ -f "$binaries/installation-hash.txt" ]
	then
		read -r installation_hash_old < "$binaries/installation-hash.txt"
	else
		installation_hash_old=""
	fi
	if [[ "$installation_hash_old" == "$installation_hash" ]]
	then
		if [ -f "$binaries/installation-failed" ]
		then
			log="$( < "$binaries/install-log.txt" )"
			# Atomic stdout:
			printf "\n\033[0;31m===>>> Install [%s] for [%s]:\033[0m\n\n%s\n\n%s\n\n" \
				"$library" \
				"$system" \
				"Previous installation failed; the installation log has been:" \
				"$log" \
				>&2
			exit 1
		fi
		exit 0
	fi
	
	# Library needs installation; delete old installation, install and write new hash:
	rm -rf "${binaries:?}/"*
	set +e # Temporarily disable "fail on error".
	set +o pipefail
	"install_$system" > "$binaries/install-log.txt" 2>&1
	exit_status=$?
	set -e # Restore "fail on error".
	set -o pipefail
	echo "$installation_hash" > "$binaries/installation-hash.txt"
	if (( exit_status == 0 ))
	then
		target_std=/dev/stdout
		log_color="\033[0;32m"
	else
		target_std=/dev/stderr
		log_color="\033[0;31m"
	fi
	log="$( < "$binaries/install-log.txt" )"
	# Atomic stdout:
	printf "\n${log_color}===>>> Install [%s] for [%s]:\033[0m\n\n%s\n" \
		"$library" \
		"$system" \
		"$log" \
		>&$target_std
	if (( exit_status != 0 ))
	then
		touch "$binaries/installation-failed"
	fi
	exit $exit_status
)

install_library()( # Encapsulated common procedure for library installation:
	# Read the library configuration and compute the hash of its current implementation:
	library="$1"
	configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$library" )"
	. "$script_dir/configure.bash" # Sourced script sets configuration!
	installation_hash="$(
		cat "$script_dir/install.bash" "${required_sources[@]/%/.scm}" |
		openssl dgst -binary -sha3-512 |
		xxd -p -c 512 )"
	
	if (( ${#selected_systems[@]} == 1 ))
	then # Avoid unnecessary spawning of background processes:
		set +e # Temporarily disable "fail on error".
		set +o pipefail	
		install_system "${selected_systems[@]}"
		joined_exit_status=$(( $? != 0 ))
		set -e # Restore "fail on error".
		set -o pipefail
	else # Run an installation co-routine for each Scheme system:
		install_pids=()
		for s in "${selected_systems[@]}"
		do
			safe_fork "install_system" "$s"
		done
		safe_join
	fi
	exit $joined_exit_status
)

##################################################### Install all selected libraries on all selected Scheme systems concurrently:
if (( ${#selected_libraries[@]} == 1 ))
then # Avoid unnecessary spawning of background processes:
	set +e # Temporarily disable "fail on error".
	set +o pipefail	
	install_library "${selected_libraries[@]}"
	joined_exit_status=$(( $? != 0 ))
	set -e # Restore "fail on error".
	set -o pipefail
else # Run an installation co-routine for each library:
	install_pids=()
	for l in "${selected_libraries[@]}"
	do
		safe_fork "install_library" "$l"
	done
	safe_join
fi

############################################################################################################## Cleanup resources:
exit $joined_exit_status # triggers 'my_exit'
