#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -e "$script_dir/nunit" ]
then
	mkdir -p "$script_dir/nunit"
	(
		cd "$script_dir/nunit"
		NuGet install NUnit
		NuGet install NUnit.Console
	)
fi

(
	cd "$script_dir/binaries"
	mono "$script_dir/nunit/NUnit.ConsoleRunner."*.*.*"/tools/nunit3-console.exe" "$script_dir/binaries/Test.dll"
)
