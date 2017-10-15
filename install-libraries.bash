#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
while getopts s:i:h opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			selected_systems+=( "$OPTARG" );;
		i)
			selected_libraries+=( `"$script_dir/list-libraries.bash" -l "$OPTARG"` );;
		h|?)
			echo "Usage: -s Scheme system (optional multi-parameter). Permitted values:" >&2
			echo "`"$script_dir/list-scheme-systems.bash" -i | sed 's/^/             /'`" >&2
			echo "          If no Scheme system is selected, the selected RACR libraries" >&2
			echo "          are installed for all available systems." >&2
			echo "       -i RACR library to install (optional multi-parameter). Permitted values:" >&2
			echo "`"$script_dir/list-libraries.bash" -k | sed 's/^/             /'`" >&2
			echo "          If no library is selected, all libraries are installed." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ ! $# -eq 0 ]
then
	echo " !!! ERROR: Unknown [$@] command line arguments !!!" >&2
	exit 2
fi

if [ -z ${selected_systems+x} ]
then
	selected_systems=`"$script_dir/list-scheme-systems.bash" -i`
fi

if [ -z ${selected_libraries+x} ]
then
	selected_libraries=`"$script_dir/list-libraries.bash" -i`
fi

############################################################################################################## Install libraries:
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "=========================================>>> Compile for Racket:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse=`"$script_dir/list-libraries.bash" -c "$l"`
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "racket" ]]
		then
			l_bin="$l/racket-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=()
			for x in ${required_libraries[@]}
			do
				lib_path+=( ++path "$x/racket-bin" )
			done
			for x in ${required_sources[@]}
			do
				plt-r6rs ${lib_path[@]} --install --collections "$l_bin" "$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "guile" ]]
then
	echo "=========================================>>> Compile for Guile:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse=`"$script_dir/list-libraries.bash" -c "$l"`
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "guile" ]]
		then
			l_bin="$l/guile-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=( --load-path="$l_bin" )
			for x in ${required_libraries[@]}
			do
				lib_path+=( --load-path="$x/guile-bin" )
			done
			for x in ${required_sources[@]}
			do
				cp -p "$x.scm" "$l_lib"
				x=`basename "$x"`
				guild compile ${lib_path[@]} --output="$l_lib/$x.go" "$l_lib/$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "chez" ]]
then
	echo "=========================================>>> Compile for Chez Scheme:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse=`"$script_dir/list-libraries.bash" -c "$l"`
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "chez" ]]
		then
			l_bin="$l/chez-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path="$l_bin"
			for x in ${required_libraries[@]}
			do
				lib_path+=":$x/chez-bin"
			done
			for x in ${required_sources[@]}
			do
				chez --libdirs "$lib_path" -q --optimize-level 3 << \
EOF
(compile-library "$x.scm" "$l_lib/`basename "$x"`.so")
EOF
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "larceny" ]]
then
	echo "=========================================>>> Compile for Larceny:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse=`"$script_dir/list-libraries.bash" -c "$l"`
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "larceny" ]]
		then
			l_bin="$l/larceny-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path="$l_bin"
			for x in ${required_libraries[@]}
			do
				lib_path+=":$x/larceny-bin"
			done
			for x in ${required_sources[@]}
			do
				x_sls="$l_lib/`basename "$x"`.sls"
				cp -p "$x.scm" "$x_sls"
				larceny --r6rs --path "$lib_path" << \
EOF
				(import (rnrs) (larceny compiler))
				(compiler-switches (quote fast-safe)) ; optimisation (even more aggressive: fast-unsafe)
				(compile-library "$x_sls")
EOF
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "ironscheme" ]]
then
	echo "=========================================>>> Compile for IronScheme:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse=`"$script_dir/list-libraries.bash" -c "$l"`
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "ironscheme" ]]
		then
			library=`basename "$l"`
			l_bin="$l/ironscheme-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path=()
			for x in ${required_libraries[@]}
			do
				lib_path+=( -I "$x/ironscheme-bin" )
			done
			to_compile="(import"
			for x in ${required_sources[@]}
			do
				source_file=`basename "$x"`
				cp -p "$x.scm" "$l_lib/$source_file.sls"
				to_compile="$to_compile ($library $source_file)"
			done
			echo "$to_compile)" > "$l_bin/compile-script.sls"
			if [ "$library" == "racr" ] # Adapt (racr core) and copy IronScheme.dll.
			then
				mv "$l_lib/core.sls" "$l_lib/core.scm"
				"$script_dir/racr-net/transcribe-racr-core.bash" "$l_lib"
				rm "$l_lib/core.scm"
				cp -p "`dirname \`which IronScheme.Console-v4.exe\``/IronScheme.dll" "$l_bin"
			fi
			# Use subshell for local directory changes via cd:
			(
			cd "$l_bin"
			echo "(compile \"$l_bin/compile-script.sls\")" | \
				mono `which IronScheme.Console-v4.exe` -nologo ${lib_path[@]}
			)
			rm -rf "$l_lib" # Force usage of compiled IronScheme dll assemblies.
			rm "$l_bin/compile-script.sls"
		fi
	done
fi
