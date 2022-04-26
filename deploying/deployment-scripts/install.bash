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

################################################################################################################ Parse arguments:
while getopts s:l:i:h opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems_array+=( "$OPTARG" )
			;;
		l)
			mapfile -O ${#selected_libraries_array[@]} -t selected_libraries_array < <(
				"$script_dir/list-libraries.bash" -l "$OPTARG" \
				|| kill -13 $$ )
			;;
		i)
			library_configuration="$( "$script_dir/list-libraries.bash" -c "$OPTARG" )"
			mapfile -O ${#selected_libraries_array[@]} -t selected_libraries_array < <(
				dirname "$library_configuration" \
				|| kill -13 $$ )
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
		"$script_dir/install.bash" -s "$system" -i "${required_libraries[@]}"
		joined_exit_status=$(( $? != 0 ))
		set -e # Restore "fail on error".
		set -o pipefail
	else # Run an installation co-routine for each required library:
		install_pids=()
		for l in "${required_libraries[@]}"
		do
			safe_fork "$script_dir/install.bash" -s "$system" -i "$l"
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
exit $joined_exit_status # triggers 'my_exit'

############################################################################################################# OLD IMPLEMENTATION:

############################################################################################################ Configure resources:
log_dir="$( "$script_dir/create-temporary.bash" -t d )"

my_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Wait for installation co-routines to cleanup first:
	wait
	# Ensure no installation failed; otherwise set error exit status:
	if compgen -G "$log_dir/*-failed" > /dev/null
	then
		exit_status=1
	fi
	# Delete installation logs:
	rm -rf "$log_dir"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

# declare -A install_pids
install_pids=()

##################################################################### Define installation procedures for specific Scheme systems:

install_exit(){
	# Capture exit status (i.e., script success or failure):
	exit_status=$?
	# Make note that installation failed:
	if (( exit_status != 0 ))
	then
		touch "$log_dir/$1-failed" 
	fi
	# Release lock:
	"$2"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}

install_chez()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-chez" )"
	trap 'install_exit chez "$mutex"' 0 1 2 3 15
	touch "$log_dir/chez-started"
	echo ""
	echo "=========================================>>> Installation report for Chez Scheme:"
	echo ""
	for l in "${selected_libraries[@]}"
	do
		configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$l" )"
		. "$script_dir/configure.bash" # Sourced script sets configuration!
		if [ ${supported_systems["chez"]+x} ]
		then
			l_bin="$l/binaries/chez"
			l_lib="$l_bin/$( basename "$l" )"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path_string="$l_bin"
			for x in "${required_libraries[@]}"
			do
				lib_path_string+=":$x/binaries/chez"
			done
			for x in "${required_sources[@]}"
			do
				x_so="$l_lib/$( basename "$x" ).so"
				chez --libdirs "$lib_path_string" -q --optimize-level 3 << \
EOF
				(compile-library "$x.scm" "$x_so")
EOF
			done
		fi
	done
)

install_guile()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-guile" )"
	trap 'install_exit guile "$mutex"' 0 1 2 3 15
	touch "$log_dir/guile-started"
	echo ""
	echo "=========================================>>> Installation report for Guile:"
	echo ""
	for l in "${selected_libraries[@]}"
	do
		configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$l" )"
		. "$script_dir/configure.bash" # Sourced script sets configuration!
		if [ ${supported_systems["guile"]+x} ]
		then
			l_bin="$l/binaries/guile"
			l_lib="$l_bin/$( basename "$l" )"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=( --load-path="$l_bin" )
			for x in "${required_libraries[@]}"
			do
				lib_path+=( --load-path="$x/binaries/guile" )
			done
			for x in "${required_sources[@]}"
			do
				cp -p "$x.scm" "$l_lib"
				x="$( basename "$x" )"
				# workaround for broken '--no-auto-compile' flag:
				old_GUILE_AUTO_COMPILE="$GUILE_AUTO_COMPILE"
				GUILE_AUTO_COMPILE=0
				guild compile --optimize=3 "${lib_path[@]}" --output="$l_lib/$x.go" "$l_lib/$x.scm"
				GUILE_AUTO_COMPILE="$old_GUILE_AUTO_COMPILE"
			done
		fi
	done
)

install_racket()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-racket" )"
	trap 'install_exit racket "$mutex"' 0 1 2 3 15
	touch "$log_dir/racket-started"
	echo ""
	echo "=========================================>>> Installation report for Racket:"
	echo ""
	for l in "${selected_libraries[@]}"
	do
		configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$l" )"
		. "$script_dir/configure.bash" # Sourced script sets configuration!
		if [ ${supported_systems["racket"]+x} ]
		then
			l_bin="$l/binaries/racket"
			l_lib="$l_bin/$( basename "$l" )"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=()
			for x in "${required_libraries[@]}"
			do
				lib_path+=( ++path "$x/binaries/racket" )
			done
			for x in "${required_sources[@]}"
			do
				plt-r6rs "${lib_path[@]}" --install --collections "$l_bin" "$x.scm"
			done
		fi
	done
)

install_larceny()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-larceny" )"
	trap 'install_exit larceny "$mutex"' 0 1 2 3 15
	touch "$log_dir/larceny-started"
	echo ""
	echo "=========================================>>> Installation report for Larceny:"
	echo ""
	for l in "${selected_libraries[@]}"
	do
		configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$l" )"
		. "$script_dir/configure.bash" # Sourced script sets configuration!
		if [ ${supported_systems["larceny"]+x} ]
		then
			l_bin="$l/binaries/larceny"
			l_lib="$l_bin/$( basename "$l" )"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path_string="$l_bin"
			for x in "${required_libraries[@]}"
			do
				lib_path_string+=":$x/binaries/larceny"
			done
			for x in "${required_sources[@]}"
			do
				x_sls="$l_lib/$( basename "$x" ).sls"
				cp -p "$x.scm" "$x_sls"
				larceny --utf8 --r6rs --path "$lib_path_string" << \
EOF
				(import (rnrs) (larceny compiler))
				(compiler-switches (quote fast-safe)) ; optimisation (even more aggressive: fast-unsafe)
				(compile-library "$x_sls")
EOF
			done
		fi
	done
)

install_sagittarius()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-sagittarius" )"
	trap 'install_exit sagittarius "$mutex"' 0 1 2 3 15
	touch "$log_dir/sagittarius-started"
	echo ""
	echo "=========================================>>> Installation report for Sagittarius Scheme:"
	echo ""
	echo "Sagittarius Scheme doesn't require any installation procedure for RACR-libraries."
)

install_ypsilon()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock_ypsilon" )"
	trap 'install_exit ypsilon "$mutex"' 0 1 2 3 15
	touch "$log_dir/ypsilon-started"
	echo ""
	echo "=========================================>>> Installation report for Ypsilon Scheme:"
	echo ""
	echo "Ypsilon Scheme doesn't require any installation procedure for RACR-libraries."
)

install_ironscheme()( # Encapsulated installation:
	mutex="$( "$script_dir/lock-files.bash" -- "$script_dir/lock-ironscheme" )"
	trap 'install_exit ironscheme "$mutex"' 0 1 2 3 15
	touch "$log_dir/ironscheme-started"
	echo ""
	echo "=========================================>>> Installation report for IronScheme:"
	echo ""
	for l in "${selected_libraries[@]}"
	do
		configuration_to_parse="$( "$script_dir/list-libraries.bash" -c "$l" )"
		. "$script_dir/configure.bash" # Sourced script sets configuration!
		if [ ${supported_systems["ironscheme"]+x} ]
		then
			library="$( basename "$l" )"
			l_bin="$l/binaries/ironscheme"
			l_lib="$l_bin/$( basename "$l" )"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=()
			for x in "${required_libraries[@]}"
			do
				lib_path+=( -I "$x/binaries/ironscheme" )
			done
			to_compile="(import"
			for x in "${required_sources[@]}"
			do
				source_file="$( basename "$x" )"
				cp -p "$x.scm" "$l_lib/$source_file.sls"
				to_compile="$to_compile ($library $source_file)"
			done
			echo "$to_compile)" > "$l_bin/compile-script.sls"
			if [ "$library" == "racr" ] # Adapt (racr core) and copy IronScheme.dll.
			then
				mv "$l_lib/core.sls" "$l_lib/core.scm"
				"$script_dir/../../racr-net/transcribe-racr-core.bash" "$l_lib"
				rm "$l_lib/core.scm"
				cp -p "$( dirname "$( command -v IronScheme.Console-v4.exe )" )/IronScheme.dll" "$l_bin"
			fi
			# Use subshell for local directory changes via cd:
			(
			cd "$l_bin"
			echo "(compile \"$l_bin/compile-script.sls\")" | \
				mono "$( command -v IronScheme.Console-v4.exe )" -nologo "${lib_path[@]}"
			)
			rm -rf "$l_lib" # Force usage of compiled IronScheme dll assemblies.
			rm "$l_bin/compile-script.sls"
		fi
	done
)

################################################################## Install libraries (concurrently for different Scheme systems):
# Start an installation co-routine for each Scheme system:
for system in "${selected_systems[@]}"
do
	"install_$system" >> "$log_dir/$system-log.txt" 2>&1 &
	install_pids["$system"]=$!
done

# Report installation progress:
progress=( "." ".." "..." )
progress_index=0
echo ""
for _ in "${install_pids[@]}"
do
	echo ""
done
while true
do
	running=0
	status_messages=()
	for system in "${!install_pids[@]}"
	do
		if kill -0 "${install_pids[$system]}" > /dev/null 2>&1
		then
			running=$(( running + 1 ))
			if [ -f "$log_dir/$system-started" ]
			then
				status="\033[0;34minstalling\033[0m"
			else
				status="\033[1;33mwaiting\033[0m"
			fi
		elif [ -f "$log_dir/$system-failed" ]
		then
			status="\033[0;31mfailed\033[0m"
		else
			status="\033[0;32msucceeded\033[0m"
		fi
		status_messages+=( "$( printf "[%-11s: %-10s]" "$system" "$status" )" )
	done
	# \r: move cursor to start of line. \033[XA: move cursor X lines up. \033[0K: delete rest if line.
	echo -e "\r\033[$(( ${#status_messages[@]} + 1 ))A\033[0KInstalling$( printf "%-3s" "${progress[$progress_index]}")"
	for message in "${status_messages[@]}"
	do
		echo -e "\r\033[0K  $message"
	done
	if (( running == 0 ))
	then
		break
	fi
	sleep 1
	progress_index=$(( ( progress_index + 1 ) % 3 ))
done
wait # Defensive programming (all co-routines should have finished anyway).

# Report final installation report for each Scheme system separately (non-interleaved):
for system in "${!install_pids[@]}"
do
	cat "$log_dir/$system-log.txt"
done
echo ""
echo "=========================================>>> Installation summary:"
echo ""
for message in "${status_messages[@]}"
do
	echo -e "  $message"
done

############################################################################################################## Cleanup resources:
exit 0 # triggers 'my_exit'
