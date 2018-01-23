#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -e "$script_dir/nunit" ]
then
	echo "NUnit already installed; delete existing installation directory '$script_dir/nunit' to reinstall."
	exit 2
fi

mkdir "$script_dir/nunit"
(
	cd "$script_dir/nunit"
	NuGet install NUnit
	NuGet install NUnit.Console
)
