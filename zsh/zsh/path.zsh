typeset -U path

# prepends
path=(
    ~/.cargo/bin
    ~/.rbenv/bin
    ~/.cabal/bin
    "$path[@]")

# appends
path+=(/usr/local/texlive/2018/bin/x86_64-linux)

export GOPATH=${HOME}/code/go

path+=(${GOPATH}/bin)

export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH

if [[ "$MACOS" ]]; then
 path+=(/Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin)
fi

if command_exists rustc; then
  # rustup component add rust-src
  export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src

  # rust libraries
  export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH
fi

# prune paths that don't exist
path=($^path(N))

hash_override
