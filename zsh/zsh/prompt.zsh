# prompt
setopt prompt_subst

# mode-aware arrow

function p_arrow {
  if [[ $KEYMAP = "vicmd" ]]; then
    echo "%F{magenta}»%f"
  else
    echo "%F{cyan}»%f"
  fi
}

# colored path

function p_colored_path {
  local slash
  slash="%F{cyan}/%f"
  echo "${${PWD/#$HOME/~}//\//$slash}"
}

# git info

function p_vcs {
  vcs_info
  echo $vcs_info_msg_0_
}

# environments:
#  - ssh
#  - virtualenv
#  - cabal sandbox

export VIRTUAL_ENV_DISABLE_PROMPT=1

function p_envs {
  local envs

  # check for cabal sandbox in parent directories, recursively
  local cabal
  cabal=( (../)#cabal.sandbox.config(N) )

  [[ -n $SSH_CONNECTION ]] && envs+="R"
  [[ -n $VIRTUAL_ENV ]] && envs+="P"
  (( $#cabal )) && envs+="H"

  [[ -n $envs ]] && echo " %F{green}[%f$envs%F{green}]%f"
}

PROMPT='
%F{blue}λ%f $(p_colored_path)$(p_vcs)$(p_envs)
$(p_arrow) '

