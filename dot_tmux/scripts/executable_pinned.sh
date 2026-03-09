#!/bin/sh
# Manage pinned tmux windows. Uses per-window @is_pinned option.
# Status bar rendering uses native tmux format strings (no shell commands) — see tmux.conf.
# Usage: pinned.sh {toggle|next|prev}

toggle() {
    current_pinned=$(tmux show-option -wqv @is_pinned)
    if [ "$current_pinned" = "1" ]; then
        tmux set-option -w -u @is_pinned
        tmux display-message "Unpinned: $(tmux display-message -p '#{window_name}')"
    else
        tmux set-option -w @is_pinned 1
        tmux display-message "Pinned: $(tmux display-message -p '#{window_name}')"
    fi
}

cycle() {
    direction="$1"
    current=$(tmux display-message -p '#{window_id}')

    # Get all pinned windows in one call
    pinned_windows=$(tmux list-windows -a -F '#{window_id}' -f '#{@is_pinned}')
    [ -z "$pinned_windows" ] && return

    # Count and find current position
    count=0
    found=-1
    for wid in $pinned_windows; do
        if [ "$wid" = "$current" ]; then
            found=$count
        fi
        count=$((count + 1))
    done

    if [ "$count" -lt 2 ] && [ "$found" -ne -1 ]; then
        # Only one pinned window and we're already on it
        return
    fi

    if [ "$found" -eq -1 ]; then
        # Not on a pinned window — jump to first
        target=$(echo "$pinned_windows" | head -1)
    else
        if [ "$direction" = "next" ]; then
            target_idx=$(( (found + 1) % count ))
        else
            target_idx=$(( (found - 1 + count) % count ))
        fi
        target=$(echo "$pinned_windows" | awk -v n="$((target_idx + 1))" 'NR==n')
    fi

    [ -n "$target" ] && tmux switch-client -t "$target"
}

case "$1" in
    toggle) toggle ;;
    next)   cycle next ;;
    prev)   cycle prev ;;
    *)      echo "Usage: $0 {toggle|next|prev}" >&2; exit 1 ;;
esac
