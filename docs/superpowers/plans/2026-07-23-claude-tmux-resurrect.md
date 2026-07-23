# Claude tmux-resurrect Integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore running Claude Code sessions (exact conversation, per pane) when tmux-resurrect/continuum restores tmux after a reboot.

**Architecture:** Claude Code's `SessionStart` hook records the session ID as a tmux pane option (`@claude_session_id`). A tmux-resurrect `post-save-layout` hook rewrites the save file so each Claude pane's saved command becomes `claude-resurrect.sh run <id>`, which resurrect relaunches on restore; the wrapper execs `claude --resume <id>` with a plain-`claude` fallback.

**Tech Stack:** POSIX sh, awk, jq, tmux pane options, tmux-resurrect hooks, chezmoi.

**Spec:** `docs/superpowers/specs/2026-07-23-claude-tmux-resurrect-design.md`

## Global Constraints

- Scripts are POSIX `sh` (`#!/bin/sh`), matching `dot_tmux/scripts/executable_claude-attention.sh` style (tab-separated tmux formats, `$TAB` variable, functions + `case` dispatcher).
- Chezmoi naming: executables in `dot_tmux/scripts/` use the `executable_` prefix; deployed path is `~/.tmux/scripts/<name>`.
- Never amend commits.
- Do NOT run `chezmoi apply` without asking the user first (repo CLAUDE.md rule).
- tmux save-file pane line format (tab-separated, 11 fields): `pane` / session_name / window_index / window_active / `:`+flags / pane_index / pane_title / `:`+cwd / pane_active / current_command / `:`+full_command.

---

### Task 1: Record Claude session ID on the tmux pane

**Files:**
- Modify: `dot_tmux/scripts/executable_claude-attention.sh` (functions `register`, `deregister`, `_cleanup_dead_sessions`)

**Interfaces:**
- Consumes: `SessionStart`/`SessionEnd` hook stdin JSON (`.session_id`), already wired in `dot_claude/settings.json`.
- Produces: pane option `@claude_session_id` containing the Claude session UUID — read by Task 2's `rewrite`.

- [ ] **Step 1: Add session-ID capture to `register()`**

Replace the existing `register()` with:

```sh
register() {
    [ -z "$TMUX_PANE" ] && return
    # SessionStart hook pipes JSON on stdin; skip when run interactively.
    if [ ! -t 0 ]; then
        session_id=$(jq -r '.session_id // empty' 2>/dev/null)
        [ -n "$session_id" ] && tmux set-option -p -t "$TMUX_PANE" @claude_session_id "$session_id" 2>/dev/null
    fi
    tmux set-option -p -t "$TMUX_PANE" @claude_attention "active" 2>/dev/null
    tmux set-option -p -t "$TMUX_PANE" @claude_pid "$PPID" 2>/dev/null
    _cleanup_dead_sessions
    return 0
}
```

- [ ] **Step 2: Unset the option in `deregister()` and `_cleanup_dead_sessions()`**

In `deregister()`, after the existing two `set-option -u` lines, add:

```sh
    tmux set-option -p -t "$TMUX_PANE" -u @claude_session_id 2>/dev/null
```

In `_cleanup_dead_sessions()`, after the two existing `set-option -u` lines inside the loop, add:

```sh
        tmux set-option -p -t "$pane_id" -u @claude_session_id 2>/dev/null
```

- [ ] **Step 3: Verify by simulating the hook**

Run (from inside any tmux pane):

```sh
echo '{"session_id":"test-uuid-123"}' | sh ~/.local/share/chezmoi/dot_tmux/scripts/executable_claude-attention.sh register
tmux show-options -p -qv @claude_session_id
```

Expected output: `test-uuid-123`

Then clean up the fake state (this pane isn't a Claude session):

```sh
sh ~/.local/share/chezmoi/dot_tmux/scripts/executable_claude-attention.sh deregister
tmux show-options -p -qv @claude_session_id   # expected: empty
```

- [ ] **Step 4: Commit**

```bash
git add dot_tmux/scripts/executable_claude-attention.sh
git commit -m "Record Claude session ID as tmux pane option on SessionStart"
```

---

### Task 2: `claude-resurrect.sh` — save-file rewrite + restore wrapper

**Files:**
- Create: `dot_tmux/scripts/executable_claude-resurrect.sh`

**Interfaces:**
- Consumes: pane option `@claude_session_id` (Task 1); resurrect save-file path passed as `$2` by the `post-save-layout` hook (Task 3).
- Produces: `claude-resurrect.sh rewrite <file>` (mutates save file in place, atomically) and `claude-resurrect.sh run <session-id>` (execs `claude --resume <id>` or falls back to `claude`).

- [ ] **Step 1: Create the script**

```sh
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

    tmux list-panes -a -F "#{session_name}${TAB}#{window_index}${TAB}#{pane_index}${TAB}#{@claude_session_id}" 2>/dev/null |
    awk -F '\t' -v OFS='\t' '
        NR == FNR {
            if ($4 != "") ids[$1 FS $2 FS $3] = $4
            next
        }
        $1 == "pane" && NF == 11 && $11 ~ /^:(.*\/)?claude( |$)/ {
            key = $2 FS $3 FS $6
            if (key in ids)
                $11 = ":" ENVIRON["HOME"] "/.tmux/scripts/claude-resurrect.sh run " ids[key]
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
    # Session file gone (or no id): never guess with --continue; start fresh.
    exec claude
}

case "$1" in
    rewrite) rewrite "$2" ;;
    run)     run "$2" ;;
    *)       echo "Usage: $0 {rewrite <save-file>|run <session-id>}" >&2; exit 1 ;;
esac
```

Notes for the implementer:
- The `$11 ~ /^:(.*\/)?claude( |$)/` guard matches `:claude`, `:claude --resume abc`, `:/opt/foo/claude ...` but NOT `:claude-resurrect.sh ...` (post-restore saves capture the exec'd `claude --resume <id>` command line, which re-matches and gets refreshed with the current ID).
- The awk `NR == FNR` block reads the live pane map from stdin (`-`); the second file is the save file.
- Glob non-match in `run()` leaves the literal pattern, `[ -e ]` fails, and we fall through to plain `claude`.

- [ ] **Step 2: Test the rewrite against a fixture (from inside tmux)**

```sh
cd /private/tmp/claude-501/-Users-jorgep--local-share-chezmoi/47a7606e-5331-403f-9650-f4f2f967516e/scratchpad
S=$(tmux display -p '#{session_name}'); W=$(tmux display -p '#{window_index}'); P=$(tmux display -p '#{pane_index}')
printf 'pane\t%s\t%s\t0\t:*\t%s\ttitle\t:/tmp\t1\t2.1.218\t:claude\n' "$S" "$W" "$P" > fixture.txt
printf 'pane\t%s\t%s\t0\t:*\t99\ttitle\t:/tmp\t0\tzsh\t:\n' "$S" "$W" >> fixture.txt
printf 'window\t%s\t%s\t1\t:*\tlayout\n' "$S" "$W" >> fixture.txt
tmux set-option -p @claude_session_id test-uuid-123
sh ~/.local/share/chezmoi/dot_tmux/scripts/executable_claude-resurrect.sh rewrite fixture.txt
cat fixture.txt
```

Expected: line 1's last field is now `:/Users/jorgep/.tmux/scripts/claude-resurrect.sh run test-uuid-123`; lines 2–3 unchanged.

Clean up: `tmux set-option -p -u @claude_session_id && rm fixture.txt`

- [ ] **Step 3: Test the run fallback logic without launching claude**

Verify the session-file check by dry inspection (don't exec claude in an automated step):

```sh
ls "$HOME"/.claude/projects/*/ | head -1   # confirm session jsonl layout exists
```

Manual test (user-visible, optional now — covered at Task 3 verification): in a scratch tmux pane run `~/.tmux/scripts/claude-resurrect.sh run <real-uuid>` and confirm it resumes; run with `run bogus-uuid` and confirm a fresh claude starts.

- [ ] **Step 4: Commit**

```bash
git add dot_tmux/scripts/executable_claude-resurrect.sh
git commit -m "Add claude-resurrect.sh: resume Claude sessions via tmux-resurrect"
```

---

### Task 3: Wire up tmux.conf and verify end-to-end save

**Files:**
- Modify: `dot_tmux.conf` (plugin block, around line 265)

**Interfaces:**
- Consumes: `claude-resurrect.sh rewrite` (Task 2).
- Produces: live resurrect/continuum configuration.

- [ ] **Step 1: Add resurrect/continuum options**

In `dot_tmux.conf`, after `set -g @continuum-restore 'on'`, add:

```tmux
set -g @continuum-save-interval '5'
set -g @resurrect-processes 'vi vim nvim emacs man less more tail top htop irssi weechat mutt "~claude-resurrect.sh"'
set -g @resurrect-hook-post-save-layout '~/.tmux/scripts/claude-resurrect.sh rewrite'
```

(The process list repeats resurrect's defaults verbatim because setting `@resurrect-processes` replaces them.)

- [ ] **Step 2: Ask the user, then apply**

Ask whether to apply. If approved:

```bash
chezmoi apply && tmux source-file ~/.tmux.conf
```

- [ ] **Step 3: Verify a real save**

Ask the user to start (or `/clear`, or resume) one Claude session so its pane has `@claude_session_id`, then force a save with `prefix + Ctrl-s` (or run `~/.tmux/plugins/tmux-resurrect/scripts/save.sh`). Then:

```sh
grep "claude-resurrect.sh run" ~/.local/share/tmux/resurrect/last
```

Expected: one line per Claude pane that has a recorded ID, each with the correct UUID (cross-check a pane with `tmux show-options -p -t <pane> -qv @claude_session_id`).

- [ ] **Step 4: Commit**

```bash
git add dot_tmux.conf
git commit -m "Wire Claude session resurrection into tmux-resurrect/continuum"
```

---

## Post-implementation notes (relay to user)

- Claude sessions already running before this change have no recorded ID until their next `SessionStart` (resume or `/clear`); until then they restore as bare shells in the right cwd.
- Full end-to-end (restore side) proves out at the next reboot or `tmux kill-server` + fresh `tmux` + `prefix + Ctrl-r`.
