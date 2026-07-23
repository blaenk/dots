# Resurrecting Claude Code sessions via tmux-resurrect

**Date:** 2026-07-23
**Status:** Approved

## Problem

tmux-resurrect + tmux-continuum restore tmux sessions/windows/panes after a
reboot, but panes that were running Claude Code come back as bare shells (or,
at best, a fresh `claude` with no history). Claude Code can resume a specific
conversation with `claude --resume <session-id>` — the session ID is stable
even if the session is renamed — but resurrect has no knowledge of it.

`claude --continue` (resume most-recent-for-cwd) is not sufficient: multiple
Claude panes routinely share a working directory (observed in real save data:
two panes in `~/code/worktrees/ziprecruiter/data`), and `--continue` would
resume the same session in both.

## Approach

Record each Claude pane's session ID as a tmux pane option via Claude Code's
`SessionStart` hook, then rewrite resurrect's save file on every save so each
Claude pane restores through a wrapper that runs `claude --resume <id>`.
Restored panes auto-launch, matching how resurrect treats other programs.

## Components

### 1. Session ID capture — `dot_tmux/scripts/executable_claude-attention.sh`

Every Claude Code hook receives `session_id` in its stdin JSON, so a shared
helper `_record_session_id()` parses it with `jq` (already a dependency of
`notify()`) and stores it on the pane:

- `tmux set-option -p -t "$TMUX_PANE" @claude_session_id "<id>"`.

The helper is called from `register()` (`SessionStart`), `busy()`
(`UserPromptSubmit`/`PostToolUse`), `done_()` (`Stop`), and `notify()`
(`Notification`) — so sessions that were already running before this change
self-register on their next activity, and the ID self-heals after `/clear`
(which mints a new one). `deregister()` (`SessionEnd`) unsets it.

### 2. Save-file rewrite + restore wrapper — new `dot_tmux/scripts/executable_claude-resurrect.sh`

One script, two subcommands:

**`rewrite <save-file>`** — invoked by resurrect's `post-save-layout` hook,
which passes the save file path and runs *before* resurrect's
dedup/symlink step:

1. Build a `session_name / window_index / pane_index → session_id` map via
   `tmux list-panes -a -F '#{session_name}\t#{window_index}\t#{pane_index}\t#{@claude_session_id}'`.
2. Rewrite each `pane` line in the save file whose full-command field (last,
   `:`-prefixed, tab-separated field) is a `claude` invocation and whose pane
   has a recorded session ID, replacing the command with
   `~/.tmux/scripts/claude-resurrect.sh run <session-id>`.
3. Write atomically (temp file + `mv`) so a mid-save crash never corrupts the
   save. The rewrite is deterministic, so resurrect's "file unchanged →
   discard" dedup keeps working.

**`run <session-id>`** — the command resurrect executes in the restored pane:

- If `~/.claude/projects/*/<session-id>.jsonl` exists: `exec claude --resume <session-id>`.
- Otherwise: `exec claude` — deliberately *not* `--continue`, so a missing ID
  never silently resumes the wrong session.

### 3. tmux.conf

```tmux
set -g @continuum-save-interval '5'
set -g @resurrect-processes 'vi vim nvim emacs man less more tail top htop irssi weechat mutt "~claude-resurrect.sh"'
set -g @resurrect-hook-post-save-layout '~/.tmux/scripts/claude-resurrect.sh rewrite'
```

The `@resurrect-processes` value must repeat resurrect's default program list
because setting the option replaces the defaults. `~claude-resurrect.sh`
matches the rewritten full command and restores it verbatim.

### 4. Repo housekeeping

`docs/` added to `.chezmoiignore` so this spec is not deployed to `~/docs`.

## Data flow

1. Claude session starts → `SessionStart` hook → `@claude_session_id` set on pane.
2. Every 5 minutes (or `prefix + Ctrl-s`) → resurrect saves →
   `post-save-layout` hook rewrites Claude pane commands to
   `claude-resurrect.sh run <id>`.
3. Reboot → tmux starts → continuum auto-restores → resurrect recreates
   panes (correct cwd) and runs the wrapper → `claude --resume <id>`.
4. Resumed Claude fires `SessionStart` again → pane option re-recorded.

## Error handling

- **Session ID missing at save time** (Claude started/cleared within the last
  save interval): pane line is left untouched; restore falls back to whatever
  `@resurrect-processes` matching does for plain `claude` — nothing, i.e. a
  bare shell in the right directory. Acceptable; `prefix + Ctrl-s` before a
  planned reboot closes the window.
- **Session file deleted before restore**: wrapper falls back to plain `claude`.
- **Non-Claude panes / panes without the option**: rewrite leaves them untouched.
- **Stale pane options** (Claude crashed without `SessionEnd`): harmless — the
  rewrite only touches panes whose *live* full command is a `claude`
  invocation at save time.

## Known limits (accepted)

- Restore snapshot is up to one save interval old (5 min after this change).
- No save-on-shutdown hook exists; manual `prefix + Ctrl-s` is the only
  guaranteed-fresh snapshot before a planned restart.
- Resuming restores the conversation, not in-flight work (running tools,
  background shells are gone).

## Testing

1. `chezmoi apply && tmux source-file ~/.tmux.conf`.
2. Open a new Claude session; confirm `tmux show -pv @claude_session_id` on
   that pane shows a UUID.
3. `prefix + Ctrl-s`; confirm `~/.local/share/tmux/resurrect/last` shows
   `claude-resurrect.sh run <uuid>` on each Claude pane line, correct per pane.
4. Run `~/.tmux/scripts/claude-resurrect.sh run <uuid>` in a scratch pane;
   confirm it resumes that exact conversation. Test the fallback with a bogus
   UUID.
5. Full end-to-end proves out at the next reboot / `tmux kill-server` +
   restore.
