typeset -U path

export GOPATH=${HOME}/code/gopath

export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH

if command_exists rustc; then
  RUST_SYSROOT=$(rustc --print sysroot)

  # rustup component add rust-src
  export RUST_SRC_PATH=${RUST_SYSROOT}/lib/rustlib/src/rust/src

  # rust libraries
  export LD_LIBRARY_PATH=${RUST_SYSROOT}/lib:$LD_LIBRARY_PATH
fi

mkdir -p ~/bin

path=(
  ~/bin

  ${GOPATH}/bin
  ~/.local/bin
  ~/.cargo/bin
  ~/.rbenv/bin
  ~/.cabal/bin
  ~/.rvm/bin
  ~/.poetry/bin

  ~/code/app/cli/bin

  ~/.texlive/2018/bin/x86_64-linux
  /Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin

  /opt/homebrew/opt/coreutils/libexec/gnubin
  /opt/homebrew/opt/findutils/libexec/gnubin
  /opt/homebrew/opt/gnu-tar/libexec/gnubin
  /opt/homebrew/opt/gnu-sed/libexec/gnubin
  /opt/homebrew/opt/gnu-getopt/bin
  /opt/homebrew/opt/gnu-indent/libexec/gnubin
  /opt/homebrew/opt/grep/libexec/gnubin

  "$path[@]"
)

# prune paths that don't exist
path=($^path(N))

manpath=(
  /usr/local/share/man
  ~/.texlive/2018/texmf-dist/doc/man

  /opt/homebrew/share/man

  /opt/homebrew/opt/coreutils/libexec/gnuman
  /opt/homebrew/opt/findutils/libexec/gnuman
  /opt/homebrew/opt/gnu-tar/libexec/gnuman
  /opt/homebrew/opt/gnu-sed/libexec/gnuman
  /opt/homebrew/opt/grep/libexec/gnuman

  "$manpath[@]"
)

manpath=($^manpath(N))

infopath=(
  /usr/local/share/info
  ~/.texlive/2018/texmf-dist/doc/info
  "$infopath[@]"
)

infopath=($^infopath(N))

# To create a custom array-to-envvar link:
# typeset -aU env_var
# typeset -T ENV_VAR env_var ' ' # Colon by default.
# export ENV_VAR

# env_var=(
#   # …
#   "$env_var[@]"
# )
