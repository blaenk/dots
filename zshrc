autoload -U compinit promptinit colors
compinit -i
promptinit
colors

# completions
unsetopt menu_complete
unsetopt flowcontrol

setopt correct nocorrectall
setopt complete_in_word
setopt always_to_end

# directories
setopt auto_name_dirs
setopt auto_cd
setopt multios
setopt cdablevarS

# prompt
setopt prompt_subst

# history
setopt append_history
setopt hist_ignore_dups
setopt share_history

# case-insensitive substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
  zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  unset CASE_SENSITIVE
else
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
fi

# tmux
alias t="tmux"
alias ta="tmux a -t"

# rbenv
eval "$(rbenv init -)"

# ls
export LSCOLORS="exfxcxdxbxegedabagacad"
alias ls="ls -pG"

# vcsinfo: thanks to github.com/sunaku/home/
autoload -Uz vcs_info

VCS_PRE="%{$fg[red]%}(%{$reset_color%}"
VCS_SUF="%{$fg[red]%})%{$reset_color%}"

AVCS_PRE="%{$fg[green]%}{%{$reset_color%}"
AVCS_SUF="%{$fg[green]%}}%{$reset_color%}"

COLON="%{$fg[red]%}:%{$reset_color%}"
SPACE=" "
DELIM=$SPACE

VCS_PROMPT="$VCS_PRE%b%u%c$VCS_SUF "
AVCS_PROMPT="$VCS_PROMPT$AVCS_PRE%a$AVCS_SUF "

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' formats $VCS_PROMPT
zstyle ':vcs_info:*' actionformats $AVCS_PROMPT
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-message
precmd() { vcs_info }

### git: Show marker (T) if there are untracked files in repository
# Make sure you have added staged to your 'formats':  %c
function +vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null ; then
        # This will show the marker if there are any untracked files in repo.
        # If instead you want to show the marker only if there are untracked
        # files in $PWD, use:
        #[[ -n $(git ls-files --others --exclude-standard) ]] ; then
        if [[ -n ${hook_com[unstaged]} ]]; then
          hook_com[unstaged]="-$DELIM${hook_com[unstaged]}"
        else
          hook_com[unstaged]="-"
        fi
        #hook_com[unstaged]+='.'
    fi
}

# proper spacing
function +vi-git-message(){
  if [[ -n ${hook_com[unstaged]} ]]; then
    if [[ -n ${hook_com[staged]} ]]; then
      hook_com[unstaged]=" ${hook_com[unstaged]}$DELIM"
    else
      hook_com[unstaged]=" ${hook_com[unstaged]}"
    fi
  else
    if [[ -n ${hook_com[staged]} ]]; then
      hook_com[staged]=" ${hook_com[staged]}"
    fi
  fi
}

# key bindings
function accept_line {
  if [[ -n "$rprompt_cached" ]]; then
    RPROMPT="$rprompt_cached"
    rprompt_cached=""
  fi
  builtin zle .accept-line
}

function zle-line-init zle-keymap-select { zle reset-prompt }

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$ARROW}/(main|viins)/}"
}

zle -N zle-line-init
zle -N zle-keymap-select
zle -N accept_line

bindkey -v
bindkey -M vicmd "^M" accept_line # Alow RETURN in vi command.
bindkey -M vicmd v edit-command-line # ESC-v to edit in an external editor.

bindkey ' ' magic-space 
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "k" history-search-backward
bindkey -M vicmd "j" history-search-forward
bindkey -M vicmd "?" history-incremental-search-backward
bindkey -M vicmd "/" history-incremental-search-forward

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

bindkey -M viins "^L" clear-screen
bindkey -M viins "^W" backward-kill-word
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^E" end-of-line
bindkey -M viins "^R" history-incremental-search-backward # allow interactive incr search, ^G or ^C to exit
bindkey -M viins "^S" history-incremental-search-forward
bindkey -M viins "^H" backward-delete-char  # vi-backward-delete-char
bindkey -M viins "^U" kill-line             # vi-kill-line
bindkey -M viins "^?" backward-delete-char  # vi-backward-delete-char

# prompt
LAMBDA="%{$fg[blue]%}λ%{$reset_color%}"
ARROW="%{$fg[blue]%}➜%{$reset_color%}"
LEFT_ARROW="%{$fg[red]%}VI %{$reset_color%}"

PROMPT='$LAMBDA ${PWD/#$HOME/~} ${vcs_info_msg_0_}${${KEYMAP/vicmd/$LEFT_ARROW}/(main|viins)/}$ARROW '

