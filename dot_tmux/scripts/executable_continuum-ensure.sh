#!/bin/sh
# Re-inject tmux-continuum's autosave trigger into status-right after a config
# reload. tmux.conf resets status-right, and continuum only re-adds its
# "#(continuum_save.sh)" when it believes no other tmux server is running. That
# check counts any in-flight `tmux` client command (hook scripts, status
# scripts), so a reload can silently leave autosave disabled until the next
# reload. Run from tmux.conf after tpm, with a short delay so tpm's background
# plugin load has finished.

script="$HOME/.tmux/plugins/tmux-continuum/scripts/continuum_save.sh"
[ -x "$script" ] || exit 0
hook="#($script)"

cur=$(tmux show-option -gqv status-right)
case "$cur" in
    *"$hook"*) exit 0 ;;
esac
tmux set-option -g status-right "${hook}${cur}"
