#!/bin/sh
# Manage pinned tmux sessions. Stores session names in @pinned (comma-separated).
# Usage: pinned.sh {toggle|next|prev|status <session_name>}

get_pinned() {
    tmux show-option -gqv @pinned
}

set_pinned() {
    tmux set-option -g @pinned "$1"
}

is_pinned() {
    case ",$1," in
        *",$2,"*) return 0 ;;
        *) return 1 ;;
    esac
}

# Remove dead sessions from the pinned list and return the cleaned list
prune_dead() {
    pinned="$1"
    live_sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null)
    result=""
    IFS=','
    for s in $pinned; do
        case "$live_sessions" in
            *"$s"*) result="${result:+$result,}$s" ;;
        esac
    done
    unset IFS
    printf '%s' "$result"
}

toggle() {
    current=$(tmux display-message -p '#{session_name}')
    pinned=$(get_pinned)

    if is_pinned "$pinned" "$current"; then
        # Remove current session
        new=""
        IFS=','
        for s in $pinned; do
            [ "$s" = "$current" ] && continue
            new="${new:+$new,}$s"
        done
        unset IFS
        set_pinned "$new"
        tmux display-message "Unpinned: $current"
    else
        # Add current session
        set_pinned "${pinned:+$pinned,}$current"
        tmux display-message "Pinned: $current"
    fi
}

cycle() {
    direction="$1"
    current=$(tmux display-message -p '#{session_name}')
    pinned=$(prune_dead "$(get_pinned)")
    set_pinned "$pinned"

    # Count pinned sessions
    if [ -z "$pinned" ]; then
        return
    fi
    count=1
    rest="$pinned"
    while case "$rest" in *,*) true ;; *) false ;; esac; do
        rest="${rest#*,}"
        count=$((count + 1))
    done
    if [ "$count" -lt 2 ]; then
        # Only one pinned session — just switch to it if we're not on it
        if ! is_pinned "$pinned" "$current"; then
            tmux switch-client -t "$pinned"
        fi
        return
    fi

    # Build index of current session in pinned list
    idx=0
    found=-1
    IFS=','
    set -- $pinned
    unset IFS
    i=0
    for s in "$@"; do
        if [ "$s" = "$current" ]; then
            found=$i
        fi
        i=$((i + 1))
    done

    if [ "$found" -eq -1 ]; then
        # Not pinned — jump to first
        tmux switch-client -t "$1"
        return
    fi

    if [ "$direction" = "next" ]; then
        target=$(( (found + 1) % count ))
    else
        target=$(( (found - 1 + count) % count ))
    fi

    i=0
    for s in "$@"; do
        if [ "$i" -eq "$target" ]; then
            tmux switch-client -t "$s"
            return
        fi
        i=$((i + 1))
    done
}

status() {
    session="$1"
    pinned=$(get_pinned)
    if is_pinned "$pinned" "$session"; then
        printf '#[fg=black,bg=yellow] pinned #[default]'
    fi
}

case "$1" in
    toggle) toggle ;;
    next)   cycle next ;;
    prev)   cycle prev ;;
    status) status "$2" ;;
    *)      echo "Usage: $0 {toggle|next|prev|status <session>}" >&2; exit 1 ;;
esac
