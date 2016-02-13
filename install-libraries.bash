#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

################################################################################################################ Parse arguments:
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
known_libraries=( "$script_dir"/racr )
known_libraries+=( $(find "$script_dir" -type f -name dependencies.txt | sed s/\\/dependencies.txt$// | grep -v /racr$) )

while getopts s:i: opt
do
	case $opt in
		s)
			"$script_dir/list-scheme-systems.bash" -s "$OPTARG"
			if [ $? -eq 0 ]
			then
				selected_systems+=( "$OPTARG" )
			else
				exit 2
			fi;;
		i)
			found=""
			for l in ${known_libraries[@]}
			do
				if `echo "$l" | grep -q "/${OPTARG}"$`
				then
					selected_libraries+=( "$l" )
					found=true
				fi
			done
			if [ -z "$found" ]
			then
				echo " !!! ERROR: Unknown [$OPTARG] library !!!" >&2
				exit 2
			fi;;
		?)
			echo "Usage: -s Scheme system (`"$script_dir/list-scheme-systems.bash" -i`)." >&2
			echo "          Several Scheme systems can be set. If no system is selected," >&2
			echo "          the libraries are installed for all available systems." >&2
			echo "       -i RACR libraries to install." >&2
			echo "          Several libraries can be set. If no library is selected," >&2
			echo "          all libraries are installed." >&2
			exit 2;;
	esac
done
shift $(( OPTIND - 1 ))

if [ -z ${selected_systems+x} ]
then
	selected_systems=`"$script_dir/list-scheme-systems.bash" -i`
	if [ ! $? -eq 0 ]
	then
		exit 2
	fi
fi

if [ -z ${selected_libraries+x} ]
then	
	selected_libraries=${known_libraries[@]}
fi

############################################################################################################## Install libraries:
if [[ " ${selected_systems[@]} " =~ "racket" ]]
then
	echo "=========================================>>> Compile for Racket:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "racket" ]]
		then
			rm -rf "$l/racket-bin"
			mkdir -p "$l/racket-bin/`basename "$l"`"
			lib_path=""
			for x in ${required_libraries[@]}
			do
				lib_path+=" ++path $x/racket-bin"
			done
			for x in ${required_sources[@]}
			do
				plt-r6rs $lib_path --install --collections "$l/racket-bin" "$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "guile" ]]
then
	echo "==========================================>>> Compile for Guile:"
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "guile" ]]
		then
			l_bin="$l/guile-bin"
			l_lib="$l_bin/`basename "$l"`"
			rm -rf "$l_bin"
			mkdir -p "$l_lib"
			lib_path="--load-path=$l_bin"
			for x in ${required_libraries[@]}
			do
				lib_path+=" --load-path=$x/guile-bin"
			done
			for x in ${required_sources[@]}
			do
				cp -p "$x.scm" "$l_lib"
				x=`basename "$x"`
				guild compile $lib_path --output="$l_lib/$x.go" "$l_lib/$x.scm"
			done
		fi
	done
fi

if [[ " ${selected_systems[@]} " =~ "larceny" ]]
then
	# Use subshell for local directory changes via cd:
	(
	echo "=========================================>>> Compile for Larceny:"
	# Create compile script:
	cd "$script_dir"
	echo "#!r6rs" > compile-stale
	echo "(import (rnrs) (larceny compiler))" >> compile-stale
	echo "(compiler-switches (quote fast-safe))" >> compile-stale # Just for optimisation. Even more aggressive: fast-unsafe
	echo "(compile-stale-libraries)" >> compile-stale
	# Compile libraries:
	for l in ${selected_libraries[@]}
	do
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "larceny" ]]
		then
			ll=`echo "$l" | rev | cut -d/ -f1 | rev` # Extract last file part of string
			cd "$l"
			rm -rf larceny-bin
			mkdir -p "larceny-bin/$ll"
			lib_path=".."
			for x in ${required_libraries[@]}
			do
				lib_path+=":$x/larceny-bin"
			done
			for x in ${required_sources[@]} 
			do
				cp -p "$x.scm" "larceny-bin/$ll/`basename "$x"`.sls"
			done
			cd "larceny-bin/$ll"
			cp -p "$script_dir/compile-stale" .
			larceny --r6rs --path $lib_path --program compile-stale
			rm compile-stale
		fi
	done
	# Delete compile script:
	cd "$script_dir"
	rm compile-stale
	)
fi

if [[ " ${selected_systems[@]} " =~ "iron-scheme" ]]
then
	echo "=========================================>>> Compile for IronScheme:"
	if [ ! -d "$script_dir/racr-net/ironscheme-bin" ] # One time setup: copy IronScheme distribution.
	then
		console=`which IronScheme.Console-v4.exe`
		cp -r "`dirname "$console"`" "$script_dir/racr-net/ironscheme-bin/"
	fi
	for l in ${selected_libraries[@]}
	do

		library=`basename "$l"`
		library_bin="$l/ironscheme-bin"
		if [ "$library" == "racr" ] # Work around for the ``empty list is null and thus invalid key for hashtable'' bug.
		then
			mkdir "$script_dir/racr-net/racr"
			# Use subshell for local directory changes via cd:
			(
			cd "$script_dir/racr-net"
			"$script_dir/racr-net/transcribe.py"
			)
			rm -rf "$library_bin"
			mkdir -p "$library_bin/$library"
			cp -p "$script_dir/racr-net/hashtable-iron-scheme-adapter.sls" "$library_bin"
			mv "$script_dir/racr-net/racr/"*.sls "$library_bin/$library"
			rm -rf "$script_dir/racr-net/racr"
			
			rm -f "$script_dir/racr-net/ironscheme-bin/hashtable-iron-scheme-adapter.dll"
			rm -f "$script_dir/racr-net/ironscheme-bin/racr.core.dll"
			rm -f "$script_dir/racr-net/ironscheme-bin/racr.testing.dll"
			echo "(import (hashtable-iron-scheme-adapter) (racr core) (racr testing))" > \
				"$library_bin/compile-script.sls"
			echo "(library-path (list \"$library_bin\")) (compile \"$library_bin/compile-script.sls\")" | \
				mono "$script_dir/racr-net/ironscheme-bin/IronScheme.Console-v4.exe"
			
			continue
		fi
		configuration_to_parse="$l/dependencies.txt"
		. "$script_dir/parse-configuration.bash" # Sourced script sets configuration!
		if [[ " ${supported_systems[@]} " =~ "iron-scheme" ]]
		then
			rm -rf "$library_bin"
			mkdir -p "$library_bin/$library"
			lib_path="(list)"
			for x in ${required_libraries[@]}
			do
				lib_path="(cons \"$x/ironscheme-bin\" $lib_path)"
			done
			lib_path="(library-path (cons \"$script_dir/racr-net/ironscheme-bin\" $lib_path))"
			to_compile="(import "
			for x in ${required_sources[@]}
			do
				source_file=`basename "$x"`
				cp -p "$x.scm" "$library_bin/$library/$source_file.sls"
				to_compile="$to_compile ($library $source_file)"
				rm -f "$script_dir/racr-net/ironscheme-bin/$library.$source_file.dll"
			done
			echo "$to_compile)" > "$library_bin/compile-script.sls"
			echo "$lib_path (compile \"$library_bin/compile-script.sls\")" | \
				mono "$script_dir/racr-net/ironscheme-bin/IronScheme.Console-v4.exe"
		fi
	done
fi
