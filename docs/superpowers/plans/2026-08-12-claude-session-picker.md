# `cs` Claude Session Picker Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A zsh function `cs` that fzf-picks any Claude Code session (live or ended), previews the chat, and on ENTER switches to the live tmux pane or `cd`s to the session's directory and runs `claude --resume`.

**Architecture:** One new file `zsh/claude.zsh` (sourced by `dot_zshrc`) holding three functions: `_cs_list` emits tab-delimited rows for the newest 50 transcripts joined against tmux pane options for liveness; `_cs_preview` renders live panes via `tmux capture-pane` and ended sessions via a jq transcript filter; `cs` wires them into fzf and handles ENTER. Spec: `docs/superpowers/specs/2026-08-12-claude-session-picker-design.md`.

**Tech Stack:** zsh (glob qualifiers, `zsh/stat`, `zsh/datetime`), fzf/fzf-tmux, jq, tmux, grep/tail.

## Global Constraints

- This is a chezmoi dotfiles repo; `zsh/` is in `.chezmoiignore` and sourced in place from `~/.local/share/chezmoi/zsh/` — editing files here is live for new shells, no `chezmoi apply` needed for `zsh/` files.
- `dot_zshrc` IS deployed by chezmoi; after modifying it the change only reaches `~/.zshrc` via `chezmoi apply` — but per repo CLAUDE.md, **ask the user before running `chezmoi apply`**.
- Transcripts live at `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl`; only top-level `*.jsonl` (subdirectories hold subagent transcripts).
- Live-session source of truth: tmux pane options `@claude_session_id` / `@claude_attention` set by `dot_tmux/scripts/executable_claude-attention.sh`. Do NOT read `~/.claude/sessions/*.json`.
- Row format (tab-delimited, exactly 6 fields): `path \t id \t cwd \t pane_id \t target \t display`. Fields 4–5 empty for ended sessions.
- Attention glyphs must match the existing status-bar vocabulary: `!` blocked, `…` busy, `✓` done, `~` idle, `●` active/live, space for ended.
- Cap: 50 newest transcripts by mtime.
- No test framework exists in this repo; every task verifies by running the code and checking output. Never commit before the verify step passes.

---

### Task 1: `_cs_list` — session list builder

**Files:**
- Create: `zsh/claude.zsh`

**Interfaces:**
- Consumes: tmux pane options `@claude_session_id`, `@claude_attention` (may be absent → all rows "ended"); `~/.claude/projects/*/*.jsonl`.
- Produces: `_cs_list` (no args) printing one row per session, newest first: `path \t id \t cwd \t pane_id \t target \t display`. `_cs_reltime <epoch>` printing a compact age (`now`, `12m`, `3h`, `2d`, `5w`). Task 3 depends on these exact names and the 6-field format.

- [ ] **Step 1: Write `zsh/claude.zsh` with `_cs_reltime` and `_cs_list`**

```zsh
# Claude Code session picker (cs): list all sessions (live or ended) across
# every project, preview the chat, switch to live panes or resume ended ones.
# Design: docs/superpowers/specs/2026-08-12-claude-session-picker-design.md

zmodload zsh/datetime
zmodload -F zsh/stat b:zstat

# Compact age from an epoch timestamp: now, 12m, 3h, 2d, 5w
_cs_reltime() {
  local delta=$(( EPOCHSECONDS - $1 ))
  if   (( delta < 60 ));     then print -r -- "now"
  elif (( delta < 3600 ));   then print -r -- "$(( delta / 60 ))m"
  elif (( delta < 86400 ));  then print -r -- "$(( delta / 3600 ))h"
  elif (( delta < 604800 )); then print -r -- "$(( delta / 86400 ))d"
  else                            print -r -- "$(( delta / 604800 ))w"
  fi
}

# One row per transcript, newest first (capped at 50):
#   path \t session-id \t cwd \t pane-id \t target \t display
# pane-id and target (session:window) are empty unless the session is live
# in a tmux pane, per the options maintained by claude-attention.sh.
_cs_list() {
  local -a files
  files=( ~/.claude/projects/*/*.jsonl(N.om[1,50]) )
  (( $#files )) || return 0

  local -A live
  local sid pane target attn
  while IFS=$'\t' read -r sid pane target attn; do
    [[ -n $sid ]] && live[$sid]=$pane$'\t'$target$'\t'$attn
  done < <(tmux list-panes -a -F $'#{@claude_session_id}\t#{pane_id}\t#{session_name}:#{window_index}\t#{@claude_attention}' 2>/dev/null)

  local f id cwd name rel glyph pane_id tgt attn_state
  local -a stat_reply
  for f in $files; do
    id=${${f:t}%.jsonl}
    cwd=$(grep -m1 '"cwd"' $f | jq -r '.cwd // empty' 2>/dev/null)
    name=$(grep '"type":"custom-title"' $f | tail -n1 |
      jq -r '.customTitle // empty' 2>/dev/null)
    if [[ -z $name ]]; then
      # Fallback label: first real user prompt (skip meta and <command-*> turns).
      name=$(grep -m5 '"type":"user"' $f |
        jq -Rr 'fromjson? | select(.isMeta != true) | .message.content? |
                if type == "string" then .
                elif type == "array" then ([.[] | select(.type? == "text") | .text] | join(" "))
                else empty end' 2>/dev/null |
        grep -v '^[[:space:]]*$' | grep -v '^<' | head -n1)
    fi
    name=${${name//[$'\t\n']/ }[1,80]}
    [[ -z $name ]] && name="(no prompt)"

    zstat -A stat_reply +mtime $f
    rel=$(_cs_reltime $stat_reply[1])

    pane_id='' tgt='' glyph=' '
    if (( ${+live[$id]} )); then
      IFS=$'\t' read -r pane_id tgt attn_state <<< "${live[$id]}"
      case $attn_state in
        blocked) glyph='!' ;;
        busy)    glyph='…' ;;
        done)    glyph='✓' ;;
        idle)    glyph='~' ;;
        *)       glyph='●' ;;
      esac
    fi

    print -r -- "$f"$'\t'"$id"$'\t'"$cwd"$'\t'"$pane_id"$'\t'"$tgt"$'\t'"$glyph ${(r:4:)rel} $name — ${cwd/#$HOME/\~}"
  done
}
```

- [ ] **Step 2: Verify the row format and sort order**

Run:
```bash
zsh -c 'source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_list' | head -8 | awk -F'\t' '{print NF, $2, $6}'
```
Expected: every line starts with `6` (field count), session IDs are UUIDs, display strings look like `● now  <title-or-prompt> — ~/...`. First rows are the most recently active sessions (this one should be at or near the top with a live glyph). Run again piped to `cut -f4,5` and confirm live rows have pane id (`%NN`) and target (`name:N`) while old rows have empty fields.

- [ ] **Step 3: Verify the ended/no-tmux path**

Run:
```bash
zsh -c 'TMUX= tmux() { return 1 }; source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_list' | head -3 | cut -f4,5
```
Expected: no errors, all pane/target fields empty (list still renders without tmux).

- [ ] **Step 4: Commit**

```bash
cd ~/.local/share/chezmoi
git add zsh/claude.zsh
git commit -m "Add _cs_list: Claude session list builder for the cs picker"
```

---

### Task 2: `_cs_preview` — chat preview renderer

**Files:**
- Modify: `zsh/claude.zsh` (append)

**Interfaces:**
- Consumes: nothing from Task 1 (standalone; only needs `tmux`, `jq`, `tail`, `tac`).
- Produces: `_cs_preview <transcript-path> <pane-id-or-empty>` printing the preview to stdout. Task 3 invokes it from fzf's `--preview` via `zsh -c "source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_preview {1} {4}"`.

- [ ] **Step 1: Append `_cs_preview` to `zsh/claude.zsh`**

```zsh
# fzf preview: live sessions render the actual tmux pane (same style as the
# claude-attention.sh pickers); ended sessions render the last 20 chat
# messages from the transcript tail. Args: <transcript-path> <pane-id|''>
_cs_preview() {
  local path=$1 pane=$2
  if [[ -n $pane ]]; then
    tmux capture-pane -t $pane -ep | tac | awk 'NF{found=1} found' | tac |
      tail -n ${FZF_PREVIEW_LINES:-40}
  else
    # -R + fromjson? skips the (possibly truncated) first line and any
    # non-JSON noise; -n so `inputs` sees every line.
    tail -c 400000 $path | jq -Rnr '
      [ inputs
        | fromjson?
        | select(.type == "user" or .type == "assistant")
        | select(.isMeta != true and .isSidechain != true)
        | { role: .type,
            text: (.message.content? |
              if type == "string" then .
              elif type == "array" then ([.[] | select(.type? == "text") | .text] | join("\n"))
              else "" end) }
        | select(.text != "" and (.text | startswith("<") | not))
        | if .role == "user"
          then "\u001b[1;36m❯\u001b[0;36m " + .text + "\u001b[0m"
          else .text
          end
      ] | .[-20:] | join("\n\n")'
  fi
}
```

- [ ] **Step 2: Verify the ended-session (jsonl) branch on a large transcript**

Run:
```bash
big=$(command ls -S ~/.claude/projects/*/*.jsonl | head -1)
time zsh -c "source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_preview $big ''" | tail -30
```
Expected: readable conversation — cyan `❯`-prefixed user messages, plain assistant messages, blank line between turns, no raw JSON, no jq errors; completes well under half a second.

- [ ] **Step 3: Verify the live-pane branch**

Run (inside tmux):
```bash
pane=$(tmux list-panes -a -F '#{@claude_session_id} #{pane_id}' | awk '$1 != "" {print $2; exit}')
zsh -c "source ~/.local/share/chezmoi/zsh/claude.zsh; FZF_PREVIEW_LINES=20 _cs_preview ignored $pane" | tail -5
```
Expected: the last lines of that Claude pane's screen content, matching what's visibly on screen.

- [ ] **Step 4: Verify graceful handling of a transcript with no text turns**

Run:
```bash
printf '{"type":"mode","mode":"normal"}\nnot json\n' > /private/tmp/claude-501/-Users-jorgep--local-share-chezmoi/a045a3a1-056d-49f9-a002-eff33e896c34/scratchpad/empty.jsonl
zsh -c "source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_preview /private/tmp/claude-501/-Users-jorgep--local-share-chezmoi/a045a3a1-056d-49f9-a002-eff33e896c34/scratchpad/empty.jsonl ''"; echo "exit=$?"
```
Expected: empty output (or a blank line), `exit=0`, no jq parse errors on stderr.

- [ ] **Step 5: Commit**

```bash
cd ~/.local/share/chezmoi
git add zsh/claude.zsh
git commit -m "Add _cs_preview: live-pane and transcript chat previews"
```

---

### Task 3: `cs` — fzf wiring, ENTER behavior, shell integration

**Files:**
- Modify: `zsh/claude.zsh` (append)
- Modify: `dot_zshrc` (add source line after `source $DOTSPATH/zsh/functions.zsh`)
- Modify: `CLAUDE.md` (add `zsh/claude.zsh` to the Zsh Structure list)

**Interfaces:**
- Consumes: `_cs_list` rows (`path \t id \t cwd \t pane_id \t target \t display`) and `_cs_preview` from Tasks 1–2, exact names and formats as defined there.
- Produces: user-facing `cs` command.

- [ ] **Step 1: Append `cs` to `zsh/claude.zsh`**

```zsh
# Pick a Claude session: switch to it if it's live in tmux, else cd to its
# directory and resume it.
cs() {
  local sel
  sel=$(_cs_list | fzf-tmux +m --exit-0 --cycle --header="claude sessions" \
    --delimiter=$'\t' --with-nth=6.. \
    --preview 'zsh -c "source ~/.local/share/chezmoi/zsh/claude.zsh; _cs_preview {1} {4}"' \
    --preview-window up:60%:wrap)
  [[ -z $sel ]] && return 0

  local path id cwd pane tgt disp
  IFS=$'\t' read -r path id cwd pane tgt disp <<< "$sel"

  if [[ -n $pane ]]; then
    if [[ -n $TMUX ]]; then
      tmux switch-client -t "$tgt" && tmux select-pane -t "$pane"
    else
      print -u2 "cs: session is live in tmux at $tgt"
      return 1
    fi
  else
    if [[ ! -d $cwd ]]; then
      print -u2 "cs: directory no longer exists: ${cwd:-unknown}"
      return 1
    fi
    cd "$cwd" && claude --resume "$id"
  fi
}
```

- [ ] **Step 2: Add the source line to `dot_zshrc`**

In `dot_zshrc`, directly after the line `source $DOTSPATH/zsh/functions.zsh` (line ~104), add:

```zsh
source $DOTSPATH/zsh/claude.zsh
```

- [ ] **Step 3: Document the file in `CLAUDE.md`**

In the repo root `CLAUDE.md`, Zsh Structure section, add this line to the file list (after the `zsh/fzf.zsh` entry):

```markdown
- `zsh/claude.zsh` — `cs` Claude session picker (list/preview/resume or switch)
```

- [ ] **Step 4: Verify the new shell wiring parses**

Run:
```bash
zsh -c 'DOTSPATH=~/.local/share/chezmoi; source $DOTSPATH/zsh/claude.zsh; whence -w cs _cs_list _cs_preview _cs_reltime'
```
Expected: all four report `: function`, no parse errors.

- [ ] **Step 5: Interactive verification (requires the user or a tmux-attached shell)**

From a NEW shell inside tmux, run `cs` and check:
1. Newest sessions nearest the prompt; this live session shows a glyph (`●`/`…`/`~`).
2. Hovering a live row previews the actual pane content; hovering an ended row previews the chat transcript.
3. ENTER on a live row switches tmux to that pane.
4. `cs` again, ENTER on an ended row: shell `cd`s to the project dir and `claude --resume` opens that conversation (exit it after confirming).
5. ENTER on a row whose cwd was deleted (any `worktrees/...` row whose dir is gone) prints `cs: directory no longer exists: ...`.
6. ESC leaves the shell untouched.

Note for subagent execution: steps 3–6 need an interactive terminal; if unavailable, verify non-interactively — `_cs_list | fzf-tmux --filter` is not sufficient for ENTER paths, so instead verify the ENTER logic by simulating: run the body's branches with a hand-built `sel` line for each case (live+TMUX unset → warns; missing cwd → warns) and report that interactive checks 1–4 remain for the user.

- [ ] **Step 6: Ask the user about `chezmoi apply`**

`dot_zshrc` changed (deployed file). Per repo convention, ask the user whether to run `chezmoi apply` (tmux reload is not needed — no tmux files changed). Do not run it unprompted.

- [ ] **Step 7: Commit**

```bash
cd ~/.local/share/chezmoi
git add zsh/claude.zsh dot_zshrc CLAUDE.md
git commit -m "Add cs: fzf Claude session picker with resume and live-pane switch"
```
