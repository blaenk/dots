# determine path to dots dir

if [[ "$OSTYPE" == darwin* ]]; then
  export MACOS=1

  hash readlink=/usr/local/bin/greadlink
fi

export DOTSPATH="$(cd $(dirname $(dirname $(readlink -f ${(%):-%N}))); pwd)"

# TODO
# Warn when expected programs/packages aren't available.

# When setting up for Emacs Tramp, set the prompt to something simple that it
# can detect and disable superfluous features in order to optimize for speed.
if [[ "$TERM" == "dumb" ]]; then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst

  if whence -w precmd >/dev/null; then
    unfunction precmd
  fi

  if whence -w preexec >/dev/null; then
    unfunction preexec
  fi

  PS1='$ '

  return
fi

if [[ -f "$DOTSPATH/.theme.dark" ]]; then
  export USE_SOLARIZED_DARK=1
fi

# if TMUX_FZF is set, we're only interested in loading the fzf functions
# everything else will just slow us down
if [[ -n "$TMUX_FZF" ]]; then
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
export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE

setopt inc_append_history
setopt hist_ignore_space
setopt append_history
setopt hist_ignore_dups
setopt share_history
setopt extendedglob
setopt hist_reduce_blanks
setopt hist_verify

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

zplug "hlissner/zsh-autopair", defer:2
zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "zsh-users/zsh-completions"

zplug "plugins/command-not-found", from:oh-my-zsh

zplug "lukechilds/zsh-nvm"
zplug "lukechilds/zsh-better-npm-completion"

zplug "knu/zsh-manydots-magic", use:manydots-magic, lazy:true

zplug "b4b4r07/enhancd", use:init.sh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load

command_exists() {
  (( $+commands[$1]))
}

# strict control over source order
sources=(
  'path'
  'brew'
  'hub'
  'chruby'
  'vcsinfo'
  'prompt'
  'completions'
  'zle'
  'functions'
  'alias'
  'linux'
  'macos'
  'gtags'
  'gnome-keyring'
  'fzf'
  'highlight'
  'enhancd'
  'kubernetes'
)

for src in $sources; do
  source $DOTSPATH/zsh/zsh/$src.zsh
done

if [[ -f ~/.zsh.local ]]; then
  source ~/.zsh.local
fi
