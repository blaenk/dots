typeset -U path

# prepends
path=(~/.rbenv/bin "$path[@]")
path=(~/.cabal/bin "$path[@]")

# appends
path+=(/usr/local/texlive/2013/bin/x86_64-linux)

# prune paths that don't exist
path=($^path(N))

export GOPATH=/home/jorge/code/go
export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH

