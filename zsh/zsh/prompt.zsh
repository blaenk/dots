# prompt
setopt prompt_subst

# prompt
LAMBDA="%{$fg[blue]%}λ%{$reset_color%}"
ARROW="%{$fg[blue]%}➜%{$reset_color%}"
LEFT_ARROW="%{$fg[red]%}VI %{$reset_color%}"
SLASH="%{$fg_bold[blue]%}/%{$reset_color%}"

function color_path() {
  print -P "${${PWD/#$HOME/~}//\//$SLASH}"
}

function vimode() {
  echo "${${KEYMAP/vicmd/$LEFT_ARROW}/(main|viins)/}"
}

if [[ -n $SSH_CONNECTION ]]; then
  SSH="%{$fg[green]%}{%{$reset_color%}%m%{$fg[green]%}}%{$reset_color%} "
fi

PROMPT='$SSH$LAMBDA ${${PWD/#$HOME/~}//\//$SLASH} ${vcs_info_msg_0_}${${KEYMAP/vicmd/$LEFT_ARROW}/(main|viins)/}$ARROW '

