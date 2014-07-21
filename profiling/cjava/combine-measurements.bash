#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

# Parse arguments:
system="*"
mode="*"
hook="*"
while getopts s:m:h:r: opt
do
	case $opt in
		s)	system="$OPTARG";;
		m)	mode="$OPTARG";;
		h)	hook="$OPTARG";;
		r)	rewrite="$OPTARG";;
		?)
			echo "Usage: -s Systems"
			echo "       -m Modes"
			echo "       -h Hooks"
			echo "       -r Rewrites"
			exit 2
	esac
done
shift $(($OPTIND - 1))

# Combine measurements tables according to given filter arguments:
cd measurements
for m in `ls -p | grep / | grep -v 'fragments\|recipes'`
do
	tail -n +3 ${m}measurements.table
done | awk \
	'BEGIN {
		split("'"$system"'", systems, " ")
		split("'"$mode"'", modes, " ")
		split("'"$hook"'", hooks, " ")
		split("'"$rewrite"'", rewrites, " ")
	}
	function my_match(filters, value)
	{
		if (filters[1] == "*")
			return 1
		else for (i in filters)
			if (filters[i] == value)
				return 1
		return 0
	}
	{
		if (my_match(systems, $1) && my_match(modes, $3) && my_match(hooks, $5) && my_match(rewrites, $7)) {
			if (! (($1, $3, $5, $7) in times))
				times[$1, $3, $5, $7] = $9
			else if ($9 < times[$1, $3, $5, $7])
				times[$1, $3, $5, $7] = $9
		}
	}
	END {
		printf "  System  |   Mode   |   Hooks  | Rewrites |   Time   \n"
		printf "----------+----------+----------+----------+----------\n"
		for (key in times) {
			split(key, separate, SUBSEP)
			printf " %-8s | %-8s | %8i | %8i | %8i \n", \
				separate[1], separate[2], separate[3], separate[4], times[key]
		}
	}' > measurements.table

cd $old_pwd
