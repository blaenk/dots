# Auto-clear idle attention on pane focus

## Context

When switching to a tmux window where Claude is idle, the cyan indicator persists in the status bar even though you're already looking at it. It should clear automatically — you've seen it, you're there.

Only `idle` should auto-clear. `blocked` should persist since Claude still needs your input even if you glance at the window.

## Files to modify

1. **`dot_tmux/scripts/executable_claude-attention.sh`** — add `clear-idle` subcommand
2. **`dot_tmux.conf`** — add `pane-focus-in` hook

## Changes

### 1. `executable_claude-attention.sh` — new `clear-idle` subcommand

The existing `clear` uses `$TMUX_PANE` (set by the shell in the pane). tmux hooks don't have that — they pass the pane ID via format strings like `#{pane_id}`. So we need a subcommand that takes an explicit pane ID argument:

```sh
clear_idle() {
    pane="$1"
    [ -z "$pane" ] && return
    attention=$(tmux show-options -p -t "$pane" -qv @claude_attention 2>/dev/null)
    [ "$attention" = "idle" ] && tmux set-option -p -t "$pane" -u @claude_attention 2>/dev/null
}
```

Add to the case statement: `clear-idle) clear_idle "$2" ;;`

### 2. `dot_tmux.conf` — `pane-focus-in` hook

Add after the claude attention keybindings block:

```tmux
set-hook -g pane-focus-in "run-shell '~/.tmux/scripts/claude-attention.sh clear-idle #{pane_id}'"
```

This fires every time any pane receives focus. The script checks if that pane has `idle` state and only then clears it — cheap no-op otherwise.

## Verification

1. Set idle on a pane: `tmux set-option -p @claude_attention idle`
2. Switch away, confirm cyan indicator shows in status bar
3. Switch back to the pane — indicator disappears
4. Set blocked: `tmux set-option -p @claude_attention blocked`
5. Switch away and back — red indicator persists
