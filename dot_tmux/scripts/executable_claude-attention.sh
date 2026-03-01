#!/bin/sh
# Claude Code attention state for tmux.
# Usage: claude-attention.sh {notify|clear|status|next|prev|select}

notify() {
    [ -z "$TMUX_PANE" ] && return
    input=$(cat)
    type=$(printf '%s' "$input" | jq -r '.notification_type // empty')
    case "$type" in
        permission_prompt|elicitation_dialog)
            tmux set-option -p -t "$TMUX_PANE" @claude_attention "blocked" ;;
        idle_prompt)
            tmux set-option -p -t "$TMUX_PANE" @claude_attention "idle" ;;
    esac
}

clear() {
    [ -z "$TMUX_PANE" ] && return
    tmux set-option -p -t "$TMUX_PANE" -u @claude_attention 2>/dev/null
}

# Collect pane_id:session:window_index:window_name:attention for panes needing attention
_attention_panes() {
    tmux list-panes -a -F '#{pane_id} #{session_name} #{window_index} #{window_name}' |
    while IFS=' ' read -r pane_id sess_name win_idx win_name; do
        attention=$(tmux show-options -p -t "$pane_id" -qv @claude_attention 2>/dev/null)
        [ -z "$attention" ] && continue
        printf '%s %s %s %s %s\n' "$pane_id" "$sess_name" "$win_idx" "$win_name" "$attention"
    done
}

status() {
    _attention_panes | while IFS=' ' read -r pane_id sess_name win_idx win_name attention; do
        case "$attention" in
            blocked) printf '#[fg=black,bg=red] %s! #[default] ' "$win_name" ;;
            idle)    printf '#[fg=black,bg=cyan] %s #[default] ' "$win_name" ;;
        esac
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
    for pane_id in $(_attention_panes | awk '{print $1}'); do
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
    _attention_panes | while IFS=' ' read -r pane_id sess_name win_idx win_name attention; do
        case "$attention" in
            blocked) label="! $sess_name:$win_name (needs input)" ;;
            idle)    label="  $sess_name:$win_name (idle)" ;;
        esac
        printf '%s\t%s:%s\n' "$label" "$sess_name" "$win_idx"
    done
}

# Select from list and switch
select_attention() {
    choice=$(list | fzf --ansi --no-sort --with-nth=1 --delimiter='\t' --prompt='Claude> ')
    [ -z "$choice" ] && return
    target=$(printf '%s' "$choice" | awk -F'\t' '{print $2}')
    [ -n "$target" ] && tmux switch-client -t "$target"
}

# Display-menu picker
menu() {
    keys="abcdefghijklmnopqrstuvwxyz"
    tmp=$(mktemp)
    _attention_panes > "$tmp"

    args=""
    key_idx=0
    while IFS=' ' read -r pane_id sess_name win_idx win_name attention; do
        case "$attention" in
            blocked) label="! $sess_name:$win_name (needs input)" ;;
            idle)    label="  $sess_name:$win_name (idle)" ;;
        esac
        key_idx=$((key_idx + 1))
        key=$(printf '%s' "$keys" | cut -c"$key_idx")
        args="$args '$label' '$key' 'switch-client -t \"$sess_name:$win_idx\"'"
    done < "$tmp"
    rm -f "$tmp"

    [ -z "$args" ] && return
    eval "tmux display-menu -T '#[align=centre]Claude Attention' $args"
}

case "$1" in
    notify) notify ;;
    clear)  clear ;;
    status) status ;;
    next)   cycle next ;;
    prev)   cycle prev ;;
    select) select_attention ;;
    menu)   menu ;;
    *)      echo "Usage: $0 {notify|clear|status|next|prev|select|menu}" >&2; exit 1 ;;
esac
