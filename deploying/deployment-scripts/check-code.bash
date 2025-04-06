#!/usr/bin/env bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

set -e
set -o pipefail
shopt -s inherit_errexit
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

################################################################################################################ Parse arguments:
if [ $# -ge 1 ]
then
	echo " !!! ERROR: Unknown [$*] command line arguments !!!" >&2
	exit 64
fi

############################################################################################################# Check Bash scripts:
bash_scripts_relative=()
mapfile -O 0 -t bash_scripts_relative < <(
	find "$script_dir/../.." -type f -iname "*.bash" \
	| sort \
	|| kill -13 $$ )
bash_scripts=()
for s in "${bash_scripts_relative[@]}"
do # Normalize Bash script paths:
	bash_scripts+=( "$( cd "$( dirname "$s" )" && pwd )/$( basename "$s")" )
done

bash_scripts_checked=0
bash_scripts_passed=0
bash_scripts_failed=0

for s in "${bash_scripts[@]}"
do
	echo "############################################################################################ Checking Bash script:"
	echo "$s"
	bash_scripts_checked=$(( bash_scripts_checked + 1 ))
	set +e
	set +o pipefail
	(
	# Disable SC2317 warnings (command appears to be unreachable): Does not work for traps, which are used extensively for
	#	safe cleanup routines, rendering SC2317 pretty useless.
	cd "$(dirname "$s")" && shellcheck \
		--shell=bash \
		--format=tty \
		--wiki-link-count=30 \
		--source-path=SCRIPTDIR \
		--external-sources \
		--severity=style \
		--check-sourced \
		--exclude=SC2317 \
		"$( basename "$s")"
	)
	shellcheck_errors=$?
	(
	cd "$(dirname "$s")" && shellharden \
		--check \
		"$( basename "$s")"
	)
	shellharden_errors=$?
	set -e
	set -o pipefail
	if (( shellcheck_errors > 0 || shellharden_errors > 0 ))
	then
		if (( shellharden_errors > 0 ))
		then
			echo "" >&2
			echo "Script violates Shellharden. Check recommendations with:" >&2
			echo "	shellharden --syntax-suggest \"$s\"" >&2
			echo "" >&2
		fi
		bash_scripts_failed=$(( bash_scripts_failed + 1 ))
	else
		bash_scripts_passed=$(( bash_scripts_passed + 1 ))
	fi
done

status_message="\
╔=====C=H=E=C=K===S=U=M=M=A=R=Y===B=A=S=H===S=C=R=I=P=T=S=====╗
║ Number of tests: $( printf "%42s" "$bash_scripts_checked" ) ║
║ Tests passed:    $( printf "%42s" "$bash_scripts_passed" ) ║
║ Tests failed:    $( printf "%42s" "$bash_scripts_failed" ) ║
╚=============================================================╝"

if ((  bash_scripts_failed > 0 ))
then
	echo "" >&2
	echo "$status_message" >&2
	echo "" >&2
else
	echo ""
	echo "$status_message"
	echo ""
fi

######################################################################################### Exit considering ALL checked artefacts:
exit $(( bash_scripts_failed > 0 ))
