# prompt
setopt prompt_subst

# prompt
LAMBDA="%{$fg[blue]%}λ%{$reset_color%}"

function arrow() {
  if [[ $KEYMAP = "vicmd" ]]; then
    echo "%{$fg[magenta]%}»%{$reset_color%}"
  else
    echo "%{$fg[cyan]%}»%{$reset_color%}"
  fi
}

function color_path() {
  SLASH="%{$fg[cyan]%}/%{$reset_color%}"
  echo "${${PWD/#$HOME/~}//\//$SLASH}"
}

function vimode() {
  echo "${${KEYMAP/vicmd/$LEFT_ARROW}/(main|viins)/}"
}

if [[ -n $SSH_CONNECTION ]]; then
  SSH=" %{$fg[green]%}R%{$reset_color%}"
fi

function vcs() {
  res=${vcs_info_msg_0_}

  if [[ -n $res ]]; then
    echo " ${res}"
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=1

function virtual_env() {
  if [[ -n $VIRTUAL_ENV ]]; then
    echo " %{$fg[green]%}ENV%{$reset_color%}"
  fi
}

PROMPT='
$LAMBDA $(color_path)$(vcs)$(virtual_env)$SSH
$(arrow) '

