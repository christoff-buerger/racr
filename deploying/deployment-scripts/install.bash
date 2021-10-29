#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

selected_systems_array=()
selected_libraries=()

################################################################################################################ Parse arguments:
while getopts s:i:h opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems_array+=( "$OPTARG" )
			;;
		i)
			mapfile -t additional_libraries < <( "$script_dir/list-libraries.bash" -l "$OPTARG" || kill -13 $$ )
			selected_libraries+=( "${additional_libraries[@]}" )
			;;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter). Permitted values:" >&2
			"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /' >&2
			echo "          If no Scheme system is selected, the selected RACR libraries" >&2
			echo "          are installed for all available systems." >&2
			echo "       -i RACR library to install (optional multi-parameter). Permitted values:" >&2
			"$script_dir/list-libraries.bash" -k | sed 's/^/             /' >&2
			echo "          If no library is selected, all libraries are installed." >&2
			exit 2
			;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 2
fi

if [[ ! -v selected_systems_array[@] ]]
then
	mapfile -t selected_systems_array < <( "$script_dir/list-scheme-systems.bash" -i || kill -13 $$ )
fi
declare -A selected_systems
selected_systems=()
for s in "${selected_systems_array[@]}"
do
	selected_systems["$s"]="$s"
done

if [[ ! -v selected_libraries[@] ]]
then
	mapfile -t selected_libraries < <( "$script_dir/list-libraries.bash" -i || kill -13 $$ )
fi

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
		exit_status=2
	fi
	# Delete installation logs:
	rm -rf "$log_dir"
	# Return captured exit status (i.e., if the original script execution succeeded or not):
	exit $exit_status
}
trap 'my_exit' 0 1 2 3 15

declare -A install_pids
install_pids=()

############################################################# Define encapsulated installation procedures for each Scheme system:
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
for system in $( "$script_dir/list-scheme-systems.bash" -i )
do
	if [ ${selected_systems["$system"]+x} ]
	then
		"install_$system" >> "$log_dir/$system-log.txt" 2>&1 &
		install_pids["$system"]=$!
	fi
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
