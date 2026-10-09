#!/bin/sh
# Debounced tmux-resurrect save, run from tmux hooks (window-unlinked,
# session-closed, client-detached) so cleanup done right before a shutdown is
# captured immediately instead of waiting for tmux-continuum's next interval
# tick, which may never come.
#
# Hook firings within the debounce window collapse into one save. The save is
# skipped if the server is gone by the time it runs (kill-server / reboot
# teardown fires session-closed per session), so a half-torn-down layout never
# overwrites the last good save.

save="$HOME/.tmux/plugins/tmux-resurrect/scripts/save.sh"
[ -x "$save" ] || exit 0

dir="${TMPDIR:-/tmp}/tmux-resurrect-save-$(id -u)"
mkdir -p "$dir" || exit 0
token="$$.$(date +%s)"
printf '%s' "$token" > "$dir/token"

(
    sleep 2
    [ "$(cat "$dir/token" 2>/dev/null)" = "$token" ] || exit 0   # superseded by a later firing
    tmux list-sessions >/dev/null 2>&1 || exit 0                 # server gone: nothing to save
    "$save" quiet
) >/dev/null 2>&1 &
