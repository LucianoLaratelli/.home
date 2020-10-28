#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

TEMP="$(nvidia-settings -q GPUCoreTemp | grep -m1 GPU | perl -ne 'print $1 if /: (\d+)\./')"

arg="${1:-}"

case "$arg" in
--poly)
	echo "GPU ${TEMP}°C"
	;;
--notify)
	notify-send --urgency=normal "GPU"
	;;
*) echo "missed" ;;
esac
