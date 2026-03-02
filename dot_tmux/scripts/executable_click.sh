#!/bin/sh
# Generic click handler for tmux status line range=user regions.
# Usage: click.sh <mouse_status_range>
# Dispatches based on prefix of the range argument.

arg="$1"

case "$arg" in
    p:*)
        # Pane target: p:%N — switch to the pane
        pane_id="${arg#p:}"
        [ -n "$pane_id" ] && tmux switch-client -t "$pane_id"
        ;;
esac
