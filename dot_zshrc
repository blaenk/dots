# determine path to dots dir

[[ -n "$ENABLE_ZPROF" ]] && zmodload zsh/zprof

if [[ -n "$ENABLE_XTRACE" ]]; then
  zmodload zsh/datetime
  setopt PROMPT_SUBST
  PS4='+$EPOCHREALTIME %N:%i> '

  logfile=$(mktemp zsh_profile.XXXXXXXX)
  echo "Logging to $logfile"
  exec 3>&2 2>$logfile

  setopt XTRACE
fi

if [[ "$OSTYPE" == darwin* ]]; then
  export MACOS=1
fi

if [[ `uname` == "Darwin" ]]; then
  export PATH="/opt/homebrew/bin:$PATH"

  if ! command -v greadlink &> /dev/null; then
    echo "You must have GNU coreutils on mac. Skipping initialization."
    return
  fi

  export DOTSPATH="$(cd $(dirname $(dirname $(greadlink -f ${(%):-%N}))); pwd)"
else
  export DOTSPATH="$(cd $(dirname $(dirname $(readlink -f ${(%):-%N}))); pwd)"
fi

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

# if TMUX_FZF is set, we're only interested in loading the fzf functions
# everything else will just slow us down
if [[ -n "$TMUX_FZF" ]]; then
  source $DOTSPATH/zsh/zsh/fzf.zsh
  return
fi

fpath=(
  "$HOME/.zsh/comp"
  "$HOME/code/app/cli/completion/zsh"
  "$DOTSPATH/zsh/zsh/comp"
  /opt/homebrew/share/zsh/site-functions
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
export HISTORY_IGNORE=""
export SAVEHIST=$HISTSIZE

setopt inc_append_history
setopt hist_ignore_space
setopt append_history
setopt hist_ignore_dups
setopt share_history
setopt extendedglob
setopt hist_reduce_blanks
setopt hist_verify
setopt no_complete_aliases
setopt auto_pushd
setopt cdable_vars

# env vars
export EDITOR=vim
export VISUAL=vim

# zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

source $DOTSPATH/zsh/zsh/completions.zsh
source $DOTSPATH/zsh/zsh/zle.zsh
source $DOTSPATH/zsh/zsh/fzf.zsh

zinit wait lucid blockf for \
      Aloxaf/fzf-tab \
      hlissner/zsh-autopair \
      kutsan/zsh-system-clipboard \
      depth'1' pick"contrib/completion/zsh" \
        docker/cli \
      pick"init.sh" \
        b4b4r07/enhancd \
      if'[[ $OSTYPE != *darwin* ]]' \
        OMZP::command-not-found \
      zsh-users/zsh-completions \
      zsh-users/zsh-syntax-highlighting

command_exists() {
  (( $+commands[$1]))
}

function_exists() {
  (( $+functions[$1]))
}

typeset -aU ldflags
typeset -T LDFLAGS ldflags ' '
export LDFLAGS

typeset -aU cppflags
typeset -T CPPFLAGS cppflags ' '
export CPPFLAGS

typeset -aU pkg_config_path
typeset -T PKG_CONFIG_PATH pkg_config_path
export PKG_CONFIG_PATH

source $DOTSPATH/zsh/zsh/path.zsh
source $DOTSPATH/zsh/zsh/vcsinfo.zsh
source $DOTSPATH/zsh/zsh/prompt.zsh
source $DOTSPATH/zsh/zsh/functions.zsh
source $DOTSPATH/zsh/zsh/nvm.zsh
source $DOTSPATH/zsh/zsh/alias.zsh
source $DOTSPATH/zsh/zsh/highlight.zsh
source $DOTSPATH/zsh/zsh/enhancd.zsh
source $DOTSPATH/zsh/zsh/linux.zsh
source $DOTSPATH/zsh/zsh/macos.zsh
source $DOTSPATH/zsh/zsh/sdkman.zsh

if [[ -f ~/.zsh.local ]]; then
  source ~/.zsh.local
fi

[[ -n "$ENABLE_ZPROF" ]] && zprof

if [[ -n "$ENABLE_XTRACE" ]]; then
  unsetopt XTRACE
  exec 2>&3 3>&-
fi
