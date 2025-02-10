#!/usr/bin/env bash
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec 3>&1 4>&2
execution_time=$(
	TIMEFORMAT=%R
	{
		time "$script_dir/../deploying/deployment-scripts/execute.bash" \
			-s "$1" \
			"${@:2}" \
			1>&3 2>&4
	} 2>&1
)
exit_status=$?
exec 3>&- 4>&-
echo "$execution_time"
exit "$exit_status"
