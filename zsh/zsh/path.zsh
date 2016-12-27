typeset -U path

# prepends
path=(
    ~/.cargo/bin
    ~/.multirust/toolchains/stable/cargo/bin
    ~/.multirust/toolchains/beta/cargo/bin
    ~/.multirust/toolchains/nightly/cargo/bin
    ~/.rbenv/bin
    ~/.cabal/bin
    "$path[@]")

# appends
path+=(/usr/local/texlive/2016/bin/x86_64-linux)

export GOPATH=${HOME}/code/go

path+=(${GOPATH}/bin)

export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH

# rustup component add rust-src
export RUST_SRC_PATH=${HOME}/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

# prune paths that don't exist
path=($^path(N))
