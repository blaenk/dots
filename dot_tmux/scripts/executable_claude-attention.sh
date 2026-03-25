#!/bin/sh
# Claude Code session tracking and attention state for tmux.
# State is stored in pane options: @claude_attention (state) and @claude_pid (Claude's PID).
# Status bar rendering uses native tmux format strings (no shell commands) — see tmux.conf.
# This script handles hooks (register/deregister/notify/busy/done/clear) and interactive
# commands (next/prev/select/menu/all-*).
# Usage: claude-attention.sh {register|deregister|notify|busy|done|clear|next|prev|select|menu|all-next|all-prev|all-select|all-menu}

TAB=$(printf '\t')

# Scan all panes, clear @claude_attention where the stored PID is dead.
# Cheap: one list-panes call + kill -0 per pane.
_cleanup_dead_sessions() {
    tmux list-panes -a -F "#{pane_id}${TAB}#{@claude_attention}${TAB}#{@claude_pid}" 2>/dev/null |
    while IFS="$TAB" read -r pane_id attention pid; do
        [ -z "$attention" ] && continue
        [ -z "$pid" ] && continue
        [ "$pane_id" = "$TMUX_PANE" ] && continue
        kill -0 "$pid" 2>/dev/null && continue
        tmux set-option -p -t "$pane_id" -u @claude_attention 2>/dev/null
        tmux set-option -p -t "$pane_id" -u @claude_pid 2>/dev/null
    done
}

register() {
    [ -z "$TMUX_PANE" ] && return
    tmux set-option -p -t "$TMUX_PANE" @claude_attention "active" 2>/dev/null
    tmux set-option -p -t "$TMUX_PANE" @claude_pid "$PPID" 2>/dev/null
    _cleanup_dead_sessions
    return 0
}

deregister() {
    [ -z "$TMUX_PANE" ] && return
    tmux set-option -p -t "$TMUX_PANE" -u @claude_attention 2>/dev/null
    tmux set-option -p -t "$TMUX_PANE" -u @claude_pid 2>/dev/null
    _cleanup_dead_sessions
    return 0
}

notify() {
    [ -z "$TMUX_PANE" ] && return
    input=$(cat)
    type=$(printf '%s' "$input" | jq -r '.notification_type // empty')
    case "$type" in
        permission_prompt|elicitation_dialog)
            tmux set-option -p -t "$TMUX_PANE" @claude_attention "blocked" 2>/dev/null ;;
        idle_prompt)
            tmux set-option -p -t "$TMUX_PANE" @claude_attention "idle" 2>/dev/null ;;
    esac
    _cleanup_dead_sessions
    return 0
}

busy() {
    [ -z "$TMUX_PANE" ] && return
    tmux set-option -p -t "$TMUX_PANE" @claude_attention "busy" 2>/dev/null
    _cleanup_dead_sessions
    return 0
}

done_() {
    [ -z "$TMUX_PANE" ] && return
    tmux set-option -p -t "$TMUX_PANE" @claude_attention "done" 2>/dev/null
    _cleanup_dead_sessions
    return 0
}

clear() {
    [ -z "$TMUX_PANE" ] && return
    attention=$(tmux show-options -p -t "$TMUX_PANE" -qv @claude_attention 2>/dev/null)
    [ "$attention" = "blocked" ] && tmux set-option -p -t "$TMUX_PANE" @claude_attention "active" 2>/dev/null
}

# Collect all panes with an active Claude session (single tmux call)
_claude_panes() {
    tmux list-panes -a -F "#{pane_id}${TAB}#{session_name}${TAB}#{window_index}${TAB}#{window_name}${TAB}#{@claude_attention}" |
    while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        [ -z "$attention" ] && continue
        printf '%s\t%s\t%s\t%s\t%s\n' "$pane_id" "$sess_name" "$win_idx" "$win_name" "$attention"
    done
}

# Collect panes needing attention (blocked/busy/idle/done)
_attention_panes() {
    _claude_panes | while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        case "$attention" in
            blocked|busy|idle|done) printf '%s\t%s\t%s\t%s\t%s\n' "$pane_id" "$sess_name" "$win_idx" "$win_name" "$attention" ;;
        esac
    done
}

_blocked_panes() {
    _claude_panes | while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        [ "$attention" = "blocked" ] && printf '%s\t%s\t%s\t%s\t%s\n' "$pane_id" "$sess_name" "$win_idx" "$win_name" "$attention"
    done
}

# Cycle through attention panes (next/prev)
cycle() {
    direction="$1"
    current_pane=$(tmux display-message -p '#{pane_id}')

    # Collect attention pane IDs into a list
    panes=""
    count=0
    found=-1
    for pane_id in $(_blocked_panes | awk -F'\t' '{print $1}'); do
        panes="${panes:+$panes }$pane_id"
        if [ "$pane_id" = "$current_pane" ]; then
            found=$count
        fi
        count=$((count + 1))
    done

    [ "$count" -eq 0 ] && return

    if [ "$found" -eq -1 ]; then
        # Not on an attention pane — jump to first
        target_pane=$(echo "$panes" | awk '{print $1}')
    else
        if [ "$direction" = "next" ]; then
            target_idx=$(( (found + 1) % count ))
        else
            target_idx=$(( (found - 1 + count) % count ))
        fi
        target_pane=$(echo "$panes" | awk -v i="$((target_idx + 1))" '{print $i}')
    fi

    # Switch to the session/window containing the target pane
    target_info=$(tmux list-panes -a -F '#{pane_id} #{session_name}:#{window_index}' | grep "^$target_pane " | awk '{print $2}')
    [ -n "$target_info" ] && tmux switch-client -t "$target_info"
}

# Output list for fzf selection
list() {
    _blocked_panes | while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        label="! $sess_name:$win_name (needs input)"
        printf '%s\t%s:%s\t%s\n' "$label" "$sess_name" "$win_idx" "$pane_id"
    done
}

# Select from list and switch
select_attention() {
    choice=$(list | fzf --header="claude attention" --select-1 +m --exit-0 --cycle \
      --delimiter='\t' --with-nth=1 \
      --preview 'tmux capture-pane -t {3} -ep | tac | awk "NF{found=1} found" | tac | tail -n $FZF_PREVIEW_LINES' \
      --preview-window down:80%:nowrap)
    [ -z "$choice" ] && return
    target=$(printf '%s' "$choice" | awk -F'\t' '{print $2}')
    [ -n "$target" ] && tmux switch-client -t "$target"
}

# Display-menu picker
menu() {
    keys="abcdefghijklmnopqrstuvwxyz"
    tmp=$(mktemp)
    _blocked_panes > "$tmp"

    args=""
    key_idx=0
    while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        label="! $sess_name:$win_name (needs input)"
        key_idx=$((key_idx + 1))
        key=$(printf '%s' "$keys" | cut -c"$key_idx")
        args="$args '$label' '$key' 'switch-client -t \"$sess_name:$win_idx\"'"
    done < "$tmp"
    rm -f "$tmp"

    [ -z "$args" ] && return
    eval "tmux display-menu -T '#[align=centre]Claude Attention' $args"
}

# Cycle through all Claude session panes (next/prev)
all_cycle() {
    direction="$1"
    current_pane=$(tmux display-message -p '#{pane_id}')

    panes=""
    count=0
    found=-1
    for pane_id in $(_claude_panes | awk -F'\t' '{print $1}'); do
        panes="${panes:+$panes }$pane_id"
        if [ "$pane_id" = "$current_pane" ]; then
            found=$count
        fi
        count=$((count + 1))
    done

    [ "$count" -eq 0 ] && return

    if [ "$found" -eq -1 ]; then
        target_pane=$(echo "$panes" | awk '{print $1}')
    else
        if [ "$direction" = "next" ]; then
            target_idx=$(( (found + 1) % count ))
        else
            target_idx=$(( (found - 1 + count) % count ))
        fi
        target_pane=$(echo "$panes" | awk -v i="$((target_idx + 1))" '{print $i}')
    fi

    target_info=$(tmux list-panes -a -F '#{pane_id} #{session_name}:#{window_index}' | grep "^$target_pane " | awk '{print $2}')
    [ -n "$target_info" ] && tmux switch-client -t "$target_info"
}

# Output list of all Claude sessions for fzf selection
all_list() {
    current_pane=$(tmux display-message -p '#{pane_id}')
    _claude_panes | while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        [ "$pane_id" = "$current_pane" ] && continue
        case "$attention" in
            blocked) label="! $sess_name:$win_name (needs input)" ;;
            busy)    label="… $sess_name:$win_name (busy)" ;;
            done)    label="✓ $sess_name:$win_name (done)" ;;
            idle)    label="~ $sess_name:$win_name (idle)" ;;
            *)       label="  $sess_name:$win_name" ;;
        esac
        printf '%s\t%s:%s\t%s\n' "$label" "$sess_name" "$win_idx" "$pane_id"
    done
}

# Select from all Claude sessions and switch
all_select() {
    choice=$(all_list | fzf --header="claude sessions" --select-1 +m --exit-0 --cycle \
      --delimiter='\t' --with-nth=1 \
      --preview 'tmux capture-pane -t {3} -ep | tac | awk "NF{found=1} found" | tac | tail -n $FZF_PREVIEW_LINES' \
      --preview-window down:80%:nowrap)
    [ -z "$choice" ] && return
    target=$(printf '%s' "$choice" | awk -F'\t' '{print $2}')
    [ -n "$target" ] && tmux switch-client -t "$target"
}

# Display-menu picker for all Claude sessions
all_menu() {
    keys="abcdefghijklmnopqrstuvwxyz"
    tmp=$(mktemp)
    _claude_panes > "$tmp"

    args=""
    key_idx=0
    while IFS="$TAB" read -r pane_id sess_name win_idx win_name attention; do
        case "$attention" in
            blocked) label="! $sess_name:$win_name (needs input)" ;;
            busy)    label="… $sess_name:$win_name (busy)" ;;
            done)    label="✓ $sess_name:$win_name (done)" ;;
            idle)    label="~ $sess_name:$win_name (idle)" ;;
            *)       label="  $sess_name:$win_name" ;;
        esac
        key_idx=$((key_idx + 1))
        key=$(printf '%s' "$keys" | cut -c"$key_idx")
        args="$args '$label' '$key' 'switch-client -t \"$sess_name:$win_idx\"'"
    done < "$tmp"
    rm -f "$tmp"

    [ -z "$args" ] && return
    eval "tmux display-menu -T '#[align=centre]Claude Sessions' $args"
}

case "$1" in
    register)   register ;;
    deregister) deregister ;;
    notify)     notify ;;
    busy)       busy ;;
    done)       done_ ;;
    clear)      clear ;;
    next)       cycle next ;;
    prev)       cycle prev ;;
    select)     select_attention ;;
    menu)       menu ;;
    all-next)   all_cycle next ;;
    all-prev)   all_cycle prev ;;
    all-select) all_select ;;
    all-menu)   all_menu ;;
    *)          echo "Usage: $0 {register|deregister|notify|busy|done|clear|next|prev|select|menu|all-next|all-prev|all-select|all-menu}" >&2; exit 1 ;;
esac
