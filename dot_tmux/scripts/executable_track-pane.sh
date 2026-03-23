#!/bin/bash
# Track the last focused pane for M-~ "last pane" navigation.
# Called by hooks with the new pane's ID as $1.
# Only updates @last_pane on the first hook to fire per transition
# (subsequent hooks see @current_pane already == new pane and skip).
new_pane=$1
cur=$(tmux show -gv @current_pane 2>/dev/null)
if [ "$cur" != "$new_pane" ]; then
  [ -n "$cur" ] && tmux set -g @last_pane "$cur"
  tmux set -g @current_pane "$new_pane"
fi
