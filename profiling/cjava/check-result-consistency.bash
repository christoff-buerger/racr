#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

ls measurements/*/result-fragments/Targets-*-h*-r*.cjava | awk \
	'BEGIN {
		FS = "-"
	}
	{
		# Only the number of hooks [$(NF-1)] and rewrites [$NF] influence composition output.
		if (! (($(NF-1), $NF) in seen)) {
			seen[$(NF-1), $NF] = 1
			printf "measurements/*/result-fragments/Targets-*\-%s\-%s\n", $(NF-1), $NF
		}
	}' | while read line
	do
		ref=`ls -1 $line | head -n 1`
		for f in $line
		do
			result=`diff --text $f $ref`
			if [ "$result" != "" ]
			then
				echo "WARNING: Composition results differ"
				echo "	$ref"
				echo "	$f"
			fi
		done
	done
