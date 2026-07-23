#!/bin/sh
# tmux-resurrect integration for Claude Code sessions.
# rewrite <save-file>: post-save-layout hook — replace each Claude pane's saved
#   command with "claude-resurrect.sh run <session-id>" using the pane option
#   @claude_session_id recorded by claude-attention.sh register.
# run <session-id>: executed in the restored pane — resume that session, or
#   start a fresh claude if the session file no longer exists.
# Usage: claude-resurrect.sh {rewrite <save-file>|run <session-id>}

TAB=$(printf '\t')

rewrite() {
    file="$1"
    [ -f "$file" ] || return 0
    tmp="${file}.claude-rewrite.$$"

    # First input: live session/window/pane -> session_id map. Second: save file.
    # Pane lines have 11 tab-separated fields; the 11th is ":<full command>".
    tmux list-panes -a -F "#{session_name}${TAB}#{window_index}${TAB}#{pane_index}${TAB}#{@claude_session_id}" 2>/dev/null |
    awk -F '\t' -v OFS='\t' '
        NR == FNR {
            if ($4 != "") ids[$1 FS $2 FS $3] = $4
            next
        }
        $1 == "pane" && NF == 11 && $11 ~ /^:(.*\/)?claude( |$)/ {
            key = $2 FS $3 FS $6
            cmd = ":" ENVIRON["HOME"] "/.tmux/scripts/claude-resurrect.sh run"
            if (key in ids) cmd = cmd " " ids[key]
            $11 = cmd
        }
        { print }
    ' - "$file" > "$tmp" && mv "$tmp" "$file" || rm -f "$tmp"
}

run() {
    id="$1"
    if [ -n "$id" ]; then
        for f in "$HOME"/.claude/projects/*/"$id".jsonl; do
            [ -e "$f" ] && exec claude --resume "$id"
        done
    fi
    # No recorded id (or session file gone): open the resume picker so the
    # unrecovered session is visible; never guess with --continue.
    exec claude --resume
}

case "$1" in
    rewrite) rewrite "$2" ;;
    run)     run "$2" ;;
    *)       echo "Usage: $0 {rewrite <save-file>|run <session-id>}" >&2; exit 1 ;;
esac
