#!/bin/bash
# Only wrap for tmux - let Claude Code handle non-tmux natively
[ -z "$TMUX" ] && exit 0

read -r input
message=$(echo "$input" | jq -r '.message // "Claude Code"')
# Must output to /dev/tty - hook stdout is captured by Claude Code
printf '\033Ptmux;\033\033]9;%s\007\033\\' "$message" > /dev/tty
