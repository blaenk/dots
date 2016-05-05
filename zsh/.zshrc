# determine path to dots dir
DOTSPATH="$(cd $(dirname $(dirname $(readlink -f ${(%):-%N}))); pwd)"

fpath=(
  "$DOTSPATH/zsh/zsh/comp"
  "${fpath[@]}"
)

autoload -U compinit bashcompinit promptinit colors select-word-style
select-word-style bash
compinit -i
bashcompinit
promptinit
colors

# history
setopt hist_ignore_space
setopt append_history
setopt hist_ignore_dups
setopt share_history
setopt extendedglob

# env vars
export EDITOR=vim
export VISUAL=vim

# bundles
if [[ ! -d $DOTSPATH/zsh/zsh/antigen ]]; then
  git clone https://github.com/zsh-users/antigen.git $DOTSPATH/zsh/zsh/antigen
fi

source $DOTSPATH/zsh/zsh/antigen/antigen.zsh

# antigen
antigen bundles <<EOBUNDLES
  zsh-users/zsh-syntax-highlighting
  zsh-users/zsh-completions src
  command-not-found
EOBUNDLES

antigen apply

# strict control over source order
sources=(
  'hub'
  'path'
  'rbenv'
  'chruby'
  'npm'
  'vcsinfo'
  'prompt'
  'completions'
  'zle'
  'highlight'
  'functions'
  'alias'
  'linux'
  'osx'
  'gtags'
  'gnome-keyring'
)

for src in $sources; do
  source $DOTSPATH/zsh/zsh/$src.zsh
done

if [[ -e $DOTSPATH/zsh/zsh/custom.zsh ]]; then
  source $DOTSPATH/zsh/zsh/custom.zsh
fi

source ~/.zsh.local
