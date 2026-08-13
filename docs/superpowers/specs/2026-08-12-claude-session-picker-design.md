# `cs` — fzf Claude session picker

Date: 2026-08-12
Status: approved

## Purpose

A zsh function `cs` that lists all Claude Code sessions (live or ended) across
every project, newest first, with a chat preview. Selecting a session either
switches to its live tmux pane or `cd`s to its directory and resumes it.

## Data sources

- **Transcripts**: `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl`.
  Only top-level `*.jsonl` files (subdirectories hold subagent transcripts).
  Relevant line types:
  - `"cwd"` field on message entries — the session's real working directory
    (the encoded directory name is lossy; never decode it).
  - `{"type":"custom-title","customTitle":"..."}` — persisted session name;
    the **last** occurrence wins.
  - `{"type":"user",...}` — first user message text is the fallback label.
- **Live sessions**: tmux pane options maintained by
  `dot_tmux/scripts/claude-attention.sh`:
  `@claude_session_id`, `@claude_attention`. One call:
  `tmux list-panes -a -F '#{@claude_session_id}<TAB>#{pane_id}<TAB>#{session_name}:#{window_index}<TAB>#{@claude_attention}'`
  maps session-id → pane. This is the same source `claude-resurrect.sh`
  trusts. Not used: `~/.claude/sessions/*.json` (would need N file reads +
  pid liveness checks).

## File layout

- New file `zsh/claude.zsh`, picked up automatically by `dot_zshrc`'s
  `$ZDOTDIR/zsh/*.zsh` glob. Contains the `cs` function and its helpers.
  No new deployed scripts; jq filters are inline.

## List building

1. Glob `~/.claude/projects/*/*.jsonl(N.om[1,50])` — newest 50 by mtime,
   pure zsh, no `ls` parsing.
2. Per file (cheap extraction; files can be multi-MB):
   - **id** = filename stem.
   - **cwd** = `grep -m1 '"cwd"'` piped to `jq -r .cwd` (stops at first match).
   - **name** = last `"type":"custom-title"` line, via `grep | tail -1 | jq`.
   - **fallback label** = first `"type":"user"` line's message text (string or
     `content[]` text item), truncated to fit one row.
3. Join against the tmux pane map to mark live sessions.
4. Row format, tab-delimited:
   `path <TAB> id <TAB> cwd <TAB> pane_id <TAB> target <TAB> display`
   where `target` is `session_name:window_index` and pane fields are empty
   for ended sessions.
5. `display` = `<glyph> <relative-time>  <name-or-prompt> — <abbreviated cwd>`
   - glyph by attention state, matching the status-bar vocabulary:
     `!` blocked, `…` busy, `✓` done, `~` idle, `●` active (live, no state
     variance), ` ` (none) for ended sessions.
   - cwd abbreviated with `~` for `$HOME`.

## fzf invocation

- `fzf-tmux +m --exit-0 --cycle --header="claude sessions"` with
  `--delimiter='\t' --with-nth=6..` so metadata fields stay invisible.
- `--preview-window up:60%:wrap`.

## Preview

The preview command branches on `{4}` (pane_id):

- **Live**: `tmux capture-pane -t {4} -ep | tac | awk 'NF{found=1} found' | tac | tail -n $FZF_PREVIEW_LINES`
  — identical to the existing pickers in `claude-attention.sh`, so both
  preview styles can be compared side by side in one picker. If the live
  style loses, switching all rows to the jsonl style is a one-line change.
- **Ended**: `tail -c 400000 {1} | jq -Rr '<filter>'` where the filter:
  - uses `fromjson?` so the possibly-truncated first line and any non-JSON
    lines are skipped silently;
  - keeps `type == "user"` / `"assistant"` entries, skipping meta entries,
    sidechains (`isSidechain == true`), and user turns whose content is only
    `tool_result` items;
  - extracts text content (user content may be a plain string or a content
    array; assistant text lives in `content[] | select(.type=="text")`);
  - takes the last 20 messages, renders `❯` + cyan for user turns, plain
    text for assistant turns, blank line between messages (ANSI escapes;
    fzf runs with `--ansi` globally).

## ENTER behavior

- **Live + inside tmux**: `tmux switch-client -t {5}` then
  `tmux select-pane -t {4}`; picker closes.
- **Live + outside tmux**: print `session is live in tmux at <target>` and
  abort — resuming a live session in a second client is worse than a no-op.
- **Ended**: if `cwd` no longer exists (deleted worktree), print a warning
  and abort. Otherwise `cd "$cwd" && claude --resume "$id"` (flows through
  the `~/bin/claude` creds wrapper automatically).

## Error handling

- No sessions on disk → `--exit-0` exits quietly; ESC → no-op.
- Corrupt/partial JSONL lines → skipped by `fromjson?`.
- Session with no user/assistant text turns → empty preview (acceptable).
- Files unreadable mid-scan → skipped.

## Testing

Manual verification:

- `cs` in this repo: sort order (newest first), live glyphs on the current
  session, name display for a renamed session, fallback label otherwise.
- Preview: live pane render vs jsonl render on a large (~1MB) transcript.
- ENTER on a live session from inside tmux (switches pane), on an ended
  session (cd + resume), and on a session whose cwd was deleted (warns).
- Outside tmux: list still renders (pane map empty, all rows ended-style),
  ENTER on a live row warns.
