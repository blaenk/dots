# prompt
setopt prompt_subst

# mode-aware arrow

function arrow {
  if [[ $KEYMAP = "vicmd" ]]; then
    echo "%F{magenta}»%f"
  else
    echo "%F{cyan}»%f"
  fi
}

# colored path

function colored_path {
  local slash
  slash="%F{cyan}/%f"
  echo "${${PWD/#$HOME/~}//\//$slash}"
}

# remote host
# not a function since, if it's a SSH session,
# it's bound to stay an SSH session, might as
# well not recompute each time

[[ -n $SSH_CONNECTION ]] && SSHP=" %F{green}R%f"

# git info

function vcs {
  vcs_info
  echo $vcs_info_msg_0_
}

# virtualenv

export VIRTUAL_ENV_DISABLE_PROMPT=1

function venv {
  [[ -n $VIRTUAL_ENV ]] && echo " %F{green}ENV%f"
}

PROMPT='
%F{blue}λ%f $(colored_path)$(vcs)$(venv)$SSHP
$(arrow) '

