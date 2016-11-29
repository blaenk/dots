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

# zplug
export ZPLUG_HOME=$DOTSPATH/zsh/zsh/zplug

if [[ ! -d $ZPLUG_HOME ]]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi

source $ZPLUG_HOME/init.zsh

zplug "zplug/zplug"

zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "zsh-users/zsh-completions"

zplug "plugins/command-not-found", from:oh-my-zsh

zplug "lukechilds/zsh-nvm"
zplug "lukechilds/zsh-better-npm-completion"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load

# strict control over source order
sources=(
  'hub'
  'path'
  'chruby'
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
