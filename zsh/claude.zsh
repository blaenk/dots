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

# fzf preview: live sessions render the actual tmux pane (same style as the
# claude-attention.sh pickers); ended sessions render the last 20 chat
# messages from the transcript tail. Args: <transcript-path> <pane-id|''>
_cs_preview() {
  local tpath=$1 pane=$2
  if [[ -n $pane ]]; then
    tmux capture-pane -t $pane -ep | tac | awk 'NF{found=1} found' | tac |
      tail -n ${FZF_PREVIEW_LINES:-40}
  else
    # -R + fromjson? skips the (possibly truncated) first line and any
    # non-JSON noise; -n so `inputs` sees every line.
    tail -c 400000 $tpath | jq -Rnr '
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
