if ! command_exists brew; then
  return 0
fi

if ! brew command command-not-found-init > /dev/null 2>&1; then
  brew tap homebrew/command-not-found
fi

eval "$(brew command-not-found-init)"

# Override certain macos utilities with the GNU coreutil versions
hash_override ls=/usr/local/bin/gls \
              wc=/usr/local/bin/gwc \
              rm=/usr/local/bin/grm \
              dircolors=/usr/local/bin/gdircolors
