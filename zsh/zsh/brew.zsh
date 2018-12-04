if ! command_exists brew; then
  return 0
fi

if ! brew command command-not-found-init > /dev/null 2>&1; then
  brew tap homebrew/command-not-found
fi

eval "$(brew command-not-found-init)"

