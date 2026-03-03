#!/bin/sh
# Manage pinned tmux windows. Stores window IDs (@1,@2,...) in @pinned (comma-separated).
# Window IDs are globally unique and stable across moves/renames.
# Usage: pinned.sh {toggle|next|prev|status <window_id>}

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

# Remove dead windows from the pinned list and return the cleaned list
prune_dead() {
    pinned="$1"
    live_windows=$(tmux list-windows -a -F '#{window_id}' 2>/dev/null)
    result=""
    IFS=','
    for w in $pinned; do
        case "$live_windows" in
            *"$w"*) result="${result:+$result,}$w" ;;
        esac
    done
    unset IFS
    printf '%s' "$result"
}

toggle() {
    current=$(tmux display-message -p '#{window_id}')
    name=$(tmux display-message -p '#{window_name}')
    pinned=$(get_pinned)

    if is_pinned "$pinned" "$current"; then
        # Remove current window
        new=""
        IFS=','
        for w in $pinned; do
            [ "$w" = "$current" ] && continue
            new="${new:+$new,}$w"
        done
        unset IFS
        set_pinned "$new"
        tmux display-message "Unpinned: $name"
    else
        # Add current window
        set_pinned "${pinned:+$pinned,}$current"
        tmux display-message "Pinned: $name"
    fi
}

cycle() {
    direction="$1"
    current=$(tmux display-message -p '#{window_id}')
    pinned=$(prune_dead "$(get_pinned)")
    set_pinned "$pinned"

    # Count pinned windows
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
        # Only one pinned window — just switch to it if we're not on it
        if ! is_pinned "$pinned" "$current"; then
            tmux switch-client -t "$pinned"
        fi
        return
    fi

    # Build index of current window in pinned list
    idx=0
    found=-1
    IFS=','
    set -- $pinned
    unset IFS
    i=0
    for w in "$@"; do
        if [ "$w" = "$current" ]; then
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
    for w in "$@"; do
        if [ "$i" -eq "$target" ]; then
            tmux switch-client -t "$w"
            return
        fi
        i=$((i + 1))
    done
}

status() {
    current_wid="$1"
    pinned=$(get_pinned)
    [ -z "$pinned" ] && return
    IFS=','
    for wid in $pinned; do
        name=$(tmux display-message -t "$wid" -p '#{window_name}' 2>/dev/null) || continue
        if [ "$wid" = "$current_wid" ]; then
            printf '#[range=user|w:%s]#[fg=black,bg=yellow] #[bg=colour15,none,fg=default,bold] %s #[norange]#[default] ' "$wid" "$name"
        else
            printf '#[range=user|w:%s]#[bg=colour0] #[bg=colour15,none,fg=default] %s #[norange]#[default] ' "$wid" "$name"
        fi
    done
    unset IFS
}

case "$1" in
    toggle) toggle ;;
    next)   cycle next ;;
    prev)   cycle prev ;;
    status) status "$2" ;;
    *)      echo "Usage: $0 {toggle|next|prev|status <window_id>}" >&2; exit 1 ;;
esac
