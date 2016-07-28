# determine path to dots dir
DOTSPATH="$(cd $(dirname $(dirname $(readlink -f ${(%):-%N}))); pwd)"

# if TMUX_FZF is set, we're only interested in loading the fzf functions
# everything else will just slow us down
if [[ ! -z "$TMUX_FZF" ]]; then
  source $DOTSPATH/zsh/zsh/fzf.zsh
  return
fi

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
  'chruby'
  'node'
  'vcsinfo'
  'prompt'
  'completions'
  'zle'
  'functions'
  'alias'
  'linux'
  'osx'
  'gtags'
  'gnome-keyring'
  'fzf'
  'highlight'
)

for src in $sources; do
  source $DOTSPATH/zsh/zsh/$src.zsh
done

source ~/.zsh.local
