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

    # First input: live session/window/pane -> cwd + session_id map. Second:
    # save file. Pane lines have 11 tab-separated fields; the 11th is
    # ":<full command>".
    tmux list-panes -a -F "#{session_name}${TAB}#{window_index}${TAB}#{pane_index}${TAB}#{pane_current_path}${TAB}#{@claude_session_id}" 2>/dev/null |
    awk -F '\t' -v OFS='\t' '
        NR == FNR {
            key = $1 FS $2 FS $3
            dirs[key] = $4
            if ($5 != "") ids[key] = $5
            next
        }
        $1 == "pane" && NF == 11 {
            key = $2 FS $3 FS $6
            # Repair lines whose empty pane title collapsed under resurrect
            # dump_panes IFS-tab read, shifting fields left by one: title
            # holds ":<dir>", dir holds the pane-active flag, and the full
            # command (looked up with the wrong pid) is lost. Restore parses
            # with the same collapsing read, so the placeholder title must be
            # non-empty. The full command is unrecoverable; leave ":" so the
            # pane restores as a shell in the right directory.
            if ($7 ~ /^:/ && $8 ~ /^[01]$/) {
                dir = (key in dirs) ? ":" dirs[key] : $7
                gsub(/ /, "\\ ", dir)
                $11 = ":"
                $10 = $9
                $9 = $8
                $8 = dir
                $7 = "-"
            }
            if ($11 ~ /^:(.*\/)?claude( |$)/) {
                cmd = ":" ENVIRON["HOME"] "/.tmux/scripts/claude-resurrect.sh run"
                if (key in ids) cmd = cmd " " ids[key]
                $11 = cmd
            }
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
