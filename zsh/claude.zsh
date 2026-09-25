# Claude Code session pickers:
#   cs  [query] — list sessions (live or ended); typing filters name/directory
#   css [query] — search every transcript with ripgrep, one row per match
#   ctrl-t inside either toggles between the two modes.
# Design: docs/superpowers/specs/2026-08-12-claude-session-picker-design.md

zmodload zsh/datetime
zmodload -F zsh/stat b:zstat

# jq fragment (for -Rn input): build an array of chat messages as MARKDOWN —
# user turns become "> ❯ …" blockquotes, assistant text passes through; glow
# does the styling downstream. fromjson? skips truncated/non-JSON lines; the
# "^<" filter drops <command-*> wrappers on user turns only, so assistant
# replies that begin with markup still render.
typeset -g _CS_JQ_MSGS='
  [ inputs
    | fromjson?
    | select(.type == "user" or .type == "assistant")
    | select(.isMeta != true and .isSidechain != true)
    | { role: .type,
        text: (.message.content? |
          if type == "string" then .
          elif type == "array" then ([.[] | select(.type? == "text") | .text] | join("\n"))
          else "" end) }
    | select(.text != "")
    | select(((.role == "user") and (.text | test("^[[:space:]]*<"))) | not)
    | if .role == "user"
      then "> ❯ " + (.text | gsub("\n"; "\n> "))
      else .text
      end
  ]'

# Markdown -> styled terminal text for previews; plain passthrough if glow
# is ever missing (the failed exec leaves stdin for cat). Prefer homebrew's
# glow: the work devbox shim earlier in PATH is old and rebuilds lazily.
_cs_md() {
  local g=/opt/homebrew/bin/glow
  [[ -x $g ]] || g=glow
  $g -s dark -w ${FZF_PREVIEW_COLUMNS:-100} - 2>/dev/null || cat
}

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

# Custom session title (set by /rename), or nothing.
_cs_title() {
  local t
  t=$(grep '"type":"custom-title"' $1 2>/dev/null | tail -n1 |
    jq -r '.customTitle // empty' 2>/dev/null)
  print -r -- ${${t//[$'\t\n']/ }[1,80]}
}

# First real user prompt (skipping meta and <command-*> turns), or nothing.
_cs_prompt1() {
  local p
  p=$(grep -m10 '"type":"user"' $1 2>/dev/null |
    jq -Rr 'fromjson? | select(.isMeta != true) | .message.content? |
            if type == "string" then .
            elif type == "array" then ([.[] | select(.type? == "text") | .text] | join(" "))
            else empty end' 2>/dev/null |
    grep -v '^[[:space:]]*$' | grep -v '^[[:space:]]*<' | head -n1)
  print -r -- ${${${p//[$'\t\n']/ }## #}[1,80]}
}

# Best single-line label: title, else first prompt (used by search rows).
_cs_label() {
  local name
  name=$(_cs_title $1)
  [[ -z $name ]] && name=$(_cs_prompt1 $1)
  print -r -- ${name:-"(no prompt)"}
}

# One row per transcript, newest first (capped at 50):
#   path \t id \t cwd \t pane-id \t target \t "glyph age" \t title \t dir \t prompt
# Fields 6-9 are displayed; fzf matches only title and dir (--nth=2,3 of the
# displayed fields) — the dimmed first-prompt column is visible but never
# matched, so typing doesn't hit prompt text. pane-id and target
# (session:window) are empty unless the session is live in a tmux pane, per
# the options maintained by claude-attention.sh.
_cs_list() {
  local -a files
  files=( ~/.claude/projects/*/*.jsonl(N.om[1,50]) )
  (( $#files )) || return 0

  local -A live
  local sid pane target attn
  while IFS=$'\t' read -r sid pane target attn; do
    [[ -n $sid ]] && live[$sid]=$pane$'\t'$target$'\t'$attn
  done < <(tmux list-panes -a -F $'#{@claude_session_id}\t#{pane_id}\t#{session_name}:#{window_index}\t#{@claude_attention}' 2>/dev/null)

  local f id cwd title prompt rel glyph pane_id tgt attn_state
  local -a stat_reply
  for f in $files; do
    [[ -r $f ]] || continue
    id=${${f:t}%.jsonl}
    cwd=$(grep -m1 '"cwd"' $f 2>/dev/null | jq -r '.cwd // empty' 2>/dev/null)
    title=$(_cs_title $f)
    [[ -z $title ]] && title=${id##*-}
    prompt=$(_cs_prompt1 $f)
    [[ -z $prompt ]] && prompt="(no prompt)"

    zstat -A stat_reply +mtime $f 2>/dev/null || continue
    rel=$(_cs_reltime $stat_reply[1])

    pane_id='' tgt='' glyph=' '
    if (( ${+live[$id]} )); then
      local -a lf=("${(@ps:\t:)live[$id]}")
      pane_id=$lf[1] tgt=$lf[2] attn_state=$lf[3]
      case $attn_state in
        blocked) glyph='!' ;;
        busy)    glyph='…' ;;
        done)    glyph='✓' ;;
        idle)    glyph='~' ;;
        *)       glyph='●' ;;
      esac
    fi

    print -r -- "$f"$'\t'"$id"$'\t'"$cwd"$'\t'"$pane_id"$'\t'"$tgt"$'\t'"$glyph ${(r:4:)rel}"$'\t'"${(r:24:)title}"$'\t'"${(r:40:)${(D)cwd}}"$'\t'$'\e[2m'"${prompt[1,60]}"$'\e[0m'
  done
}

# fzf reload command for transcript-search mode: one row per match across
# EVERY transcript (uncapped, unlike the list). Matches count only when the
# pattern appears in RENDERED chat text (what you saw in the session), not
# in raw-JSONL noise: rg finds cheap raw candidates, jq renders just those
# lines, and rows whose rendered text no longer matches are dropped.
# Rows mirror the list shape (age/name/dir columns, then the snippet) but
# carry the match's line number in field 2 (numeric ⇒ match row):
#   path \t lineno \t id \t "" \t "" \t age \t name \t dir \t snippet
_css_search() {
  local q=$1
  (( ${#q} >= 3 )) || return 0
  local -A labels ages dirs want
  local -a sr order
  local f lineno rest id text

  # 1. Raw candidates: (file, lineno) pairs, newest transcripts first
  #    (-j1 keeps rg output in argument order; -o so huge lines stay put).
  while IFS=: read -r f lineno rest; do
    [[ $lineno == <-> ]] || continue
    (( ${+want[$f]} )) || order+=($f)
    [[ " ${want[$f]} " == *" $lineno "* ]] || want[$f]+=" $lineno"
  done < <(rg -in -j1 --max-count 500 -o -e $q \
             ~/.claude/projects/*/*.jsonl(N.om) 2>/dev/null | head -n 3000)
  (( $#order )) || return 0

  # 2. Pull candidate lines (tab-prefixed with file and lineno; valid JSON
  #    never contains a raw tab byte), render them, and keep only lines
  #    whose visible chat text still matches the pattern; awk emits a
  #    snippet around the rendered match.
  # perl, not awk: BSD awk needs ~100ms per multi-MB transcript here, perl
  # ~17ms. Inputs ride in env vars (a large `perl -s` switch value gets the
  # process killed, oddly). The length guard skips huge lines (paste blobs/
  # attachments embedding message-looking text) whose rendering dominated
  # search latency.
  for f in $order; do
    CS_WANT="${want[$f]}" CS_FN=$f perl -ne '
      BEGIN { %w = map { $_ => 1 } split " ", $ENV{CS_WANT};
              $n = keys %w; $fn = $ENV{CS_FN} }
      if ($w{$.}) {
        print "$fn\t$.\t$_" if length() < 65536 && /"type":"(user|assistant)"/;
        exit unless --$n;
      }' $f
  done |
  jq -Rr 'split("\t") | .[0] as $f | .[1] as $ln | (.[2] | fromjson?)
    | select(.type == "user" or .type == "assistant")
    | select(.isMeta != true and .isSidechain != true)
    | (.message.content? |
        if type == "string" then .
        elif type == "array" then ([.[] | select(.type? == "text") | .text] | join(" "))
        else "" end) as $t
    | select($t != "")
    | $f + "\t" + $ln + "\t" + ($t | gsub("[\\n\\t]"; " "))' 2>/dev/null |
  awk -F'\t' -v pat="${(L)q}" '
    match(tolower($3), pat) {
      s = RSTART - 50; if (s < 1) s = 1
      print $1 "\t" $2 "\t" substr($3, s, RLENGTH + 100)
    }' 2>/dev/null |
  head -n 200 |
  while IFS=$'\t' read -r f lineno text; do
    id=${${f:t}%.jsonl}
    if (( ! ${+labels[$f]} )); then
      # Same identity columns as the session list: title (or the session
      # id's last 4 chars) and directory. First-prompt fallback is skipped
      # to keep broad searches (many files) snappy.
      local t=$(_cs_title $f)
      labels[$f]=${t:-${id##*-}}
      dirs[$f]=${(D)$(grep -m1 '"cwd"' $f 2>/dev/null | jq -r '.cwd // empty' 2>/dev/null)}
      zstat -A sr +mtime $f 2>/dev/null || sr=($EPOCHSECONDS)
      ages[$f]=$(_cs_reltime $sr[1])
    fi
    print -r -- "$f"$'\t'"$lineno"$'\t'"$id"$'\t\t\t'"${(r:4:)ages[$f]}"$'\t'"${(r:16:)labels[$f][1,16]}"$'\t'"${(r:32:)dirs[$f][-32,-1]}"$'\t'"$text"
  done
}

# Preview an ended/any session: last 20 chat messages from the transcript
# tail, or (with a pane id) the live tmux pane, same style as the
# claude-attention.sh pickers. Args: <transcript-path> <pane-id|''>
_cs_preview() {
  local tpath=$1 pane=$2
  if [[ -n $pane ]]; then
    tmux capture-pane -t $pane -ep | tac | awk 'NF{found=1} found' | tac |
      tail -n ${FZF_PREVIEW_LINES:-40}
  else
    tail -c 400000 $tpath | jq -Rnr "$_CS_JQ_MSGS | .[-20:] | join(\"\n\n\")" |
      _cs_md
  fi
}

# Preview a transcript-search match: the conversation around the matched
# line (±8 JSONL entries), with the query highlighted.
# Args: <transcript-path> <lineno> <query>
_css_preview() {
  local tpath=$1 line=$2 q=$3
  local s=$(( line - 8 ))
  (( s < 1 )) && s=1
  awk -v s=$s -v e=$(( line + 8 )) 'NR>=s && NR<=e' $tpath |
    jq -Rnr "$_CS_JQ_MSGS | join(\"\n\n\")" | _cs_md |
    { if [[ -n $q ]]; then rg --passthru --color=always -i -- $q 2>/dev/null || cat; else cat; fi }
}

# Unified fzf preview: match rows carry a numeric line number in field 2,
# session rows a UUID. Args: $1={1}=path $2={2}=id-or-lineno $3={4}=pane
# $4={q}=query — the query is NEVER a tmux target; only $3 is.
_cs_preview_row() {
  if [[ $2 == <-> ]]; then
    _css_preview $1 $2 $4
  else
    _cs_preview $1 $3
  fi
}

# Open a session by id: switch to its tmux pane if live (looked up fresh —
# liveness can change while the picker is open), else cd to its recorded
# cwd and resume it.
_cs_open() {
  local id=$1 tpath=$2 hit cwd
  hit=$(tmux list-panes -a -F $'#{@claude_session_id}\t#{pane_id}\t#{session_name}:#{window_index}' 2>/dev/null |
    grep -m1 "^${id}"$'\t')
  if [[ -n $hit ]]; then
    local -a hf=("${(@ps:\t:)hit}")
    if [[ -n $TMUX ]]; then
      tmux switch-client -t "$hf[3]" && tmux select-pane -t "$hf[2]"
    else
      print -u2 "cs: session is live in tmux at $hf[3]"
      return 1
    fi
    return 0
  fi
  cwd=$(grep -m1 '"cwd"' $tpath 2>/dev/null | jq -r '.cwd // empty' 2>/dev/null)
  if [[ ! -d $cwd ]]; then
    print -u2 "cs: directory no longer exists: ${cwd:-unknown}"
    return 1
  fi
  cd "$cwd" && claude --resume "$id"
}

# Shared picker. Mode 'list': _cs_list rows, typing fuzzy-filters session
# name + directory only (--nth=2,3 of the displayed fields). Mode 'rg':
# search disabled, typing is a live ripgrep pattern over all transcripts.
# ctrl-t toggles; the transform branches on the prompt to know the mode.
_cs_picker() {
  local mode=$1 q=$2 sel
  local zsrc='source ~/.local/share/chezmoi/zsh/claude.zsh'
  # The query travels as \$FZF_QUERY (expanded by the inner zsh, which does
  # not word-split), NEVER as {q}: fzf interpolates {q} inside the ctrl-t
  # transform script too, where its quoting collides with the echoed action
  # string and a query with spaces gets word-split (e.g. "job health"
  # searched as "job").
  local list_cmd="zsh -c \"$zsrc; _cs_list\""
  local rg_cmd="zsh -c \"$zsrc; _css_search \\\$FZF_QUERY\""
  local toggle="ctrl-t:transform:if [ \"\$FZF_PROMPT\" = 'rg> ' ]; then
      echo 'change-prompt(sessions> )+enable-search+unbind(change)+reload($list_cmd)'
    else
      echo 'change-prompt(rg> )+disable-search+rebind(change)+reload($rg_cmd)'
    fi"

  local -a args
  args=( +m --cycle
    --delimiter=$'\t' --with-nth=6.. --nth=2,3
    --header='enter: open · ctrl-t: toggle rg/list · rg needs ≥3 chars'
    --preview "zsh -c \"$zsrc; _cs_preview_row {1} {2} {4} {q}\""
    --preview-window down:80%:nowrap
    --query "$q"
    --bind "change:reload:sleep 0.15; $rg_cmd"
    --bind "$toggle" )
  if [[ $mode == rg ]]; then
    # No --exit-0 here: rg mode legitimately starts empty until you type.
    args+=( --disabled --prompt 'rg> ' --bind "start:reload($rg_cmd)" )
  else
    args+=( --exit-0 --prompt 'sessions> ' --bind "start:unbind(change)+reload($list_cmd)" )
  fi

  # "${(@)args}" (not $args): unquoted expansion would drop the empty
  # element from --query "" and misalign every following option.
  sel=$(fzf "${(@)args}" < /dev/null)
  [[ -z $sel ]] && return 0

  # ${(@ps:\t:)} rather than IFS read: zsh `read` collapses adjacent tabs,
  # misaligning rows with empty fields. (And never `local path` — it shadows
  # zsh's tied array form of $PATH.)
  local -a fields=("${(@ps:\t:)sel}")
  local id=$fields[2]
  [[ $id == <-> ]] && id=$fields[3]
  _cs_open "$id" "$fields[1]"
}

# Pick a Claude session (filter by name/directory; ctrl-t for content search).
cs() { _cs_picker list "$*" }

# Search all Claude transcripts with ripgrep (ctrl-t back to the list).
css() { _cs_picker rg "$*" }
