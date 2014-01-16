#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

if [ ! $# == 1 ]
then
	echo "Wrong number of arguments - one argument expected:"
	echo "	(1) Version number of the RACR source code distribution"
	exit 1
fi

# Collect all sources:
find . |
# Ignore documentation sources:
grep -v documentation |
# Ignore hidden directories and files:
grep -v "/\." |
# Zip all together with the manual:
xargs zip -0 racr-v$1.zip documentation/racr-manual.pdf
