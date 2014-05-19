# vcsinfo: thanks to github.com/sunaku/home/
autoload -Uz vcs_info

VCS_PROMPT=" %Bon %F{green}%b%F{cyan}%u%F{magenta}%c%f%m%%b"
AVCS_PROMPT="$VCS_PROMPT %Bdoing %F{magenta}%a%f%%b"

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr "'"
zstyle ':vcs_info:*' formats $VCS_PROMPT
zstyle ':vcs_info:*' actionformats $AVCS_PROMPT
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*+set-message:*' hooks git-aheadbehind git-untracked git-message

### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
# Make sure you have added misc to your 'formats':  %m
function +vi-git-aheadbehind() {
  local ahead behind
  local -a gitstatus

  behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
  (( $behind )) && gitstatus+=( " behind %F{red}${behind}%f" )

  ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
  (( $ahead )) && gitstatus+=( " ahead %F{blue}${ahead}%f" )

  hook_com[misc]+=${(j::)gitstatus}
}

### git: Show marker (T) if there are untracked files in repository
# Make sure you have added staged to your 'formats':  %c
function +vi-git-untracked(){
  if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
    git status --porcelain | grep '??' &> /dev/null ; then
    # This will show the marker if there are any untracked files in repo.
    hook_com[branch]="%F{cyan}.%F{green}${hook_com[branch]}%f"
  fi
}

# proper spacing
function +vi-git-message(){
  if [[ -n ${hook_com[unstaged]} ]]; then
    if [[ -n ${hook_com[staged]} ]]; then
      hook_com[unstaged]="${hook_com[unstaged]} "
    else
      hook_com[unstaged]="${hook_com[unstaged]}"
    fi
  else
    if [[ -n ${hook_com[staged]} ]]; then
      hook_com[staged]=" ${hook_com[staged]}"
    fi
  fi
}

