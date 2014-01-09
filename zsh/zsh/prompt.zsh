# prompt
setopt prompt_subst

# prompt
LAMBDA="%{$fg[blue]%}λ%{$reset_color%}"
ARROW="%{$fg[blue]%}→%{$reset_color%}"
LEFT_ARROW="%{$fg_bold[red]%}VI %{$reset_color%}"
SLASH="%{$fg[blue]%}/%{$reset_color%}"

function color_path() {
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

  if [[ " " != res ]]; then
    echo " ${res}"
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=1

function virtual_env() {
  if [[ -n $VIRTUAL_ENV ]]; then
    # echo " %{$fg[green]%}{%{$reset_color%}%{$fg[green]%}}%{$reset_color%}"
    echo " %{$fg[green]%}ENV%{$reset_color%}"
  fi
}

PROMPT='
$LAMBDA $(color_path)$SSH$(vcs)$(virtual_env)
$(vimode)$ARROW '

