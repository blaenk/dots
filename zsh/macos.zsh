# iterm2_prompt_mark() {
#   printf "\033]133;A\007"
# }

# # Mark end of prompt
# iterm2_prompt_end() {
#   printf "\033]133;B\007"
# }

# If this ever has problems, read https://gitlab.com/gnachman/iterm2/-/issues/10537
# The alternative is to establish a trigger on the iTerm side to detect the prompt.
#
# It seems someone also managed to get it to work with a `[custom.iterm_prompt_mark]`
# prompt component, but they may have forgotten to add the iterm_prompt_mark_end
# component, which apparently cause cmd+shift+a to select the start of the prompt

export ITERM2_SQUELCH_MARK=1

ITERM_INTEGRATION="$DOTSPATH/zsh/iterm.zsh"

if [ ! -f "${ITERM_INTEGRATION}" ]; then
  curl -s -L 'https://iterm2.com/shell_integration/zsh' -o "${ITERM_INTEGRATION}"

  # Apply iterm2_prompt_mark patch
  # patch -s -d "$DOTSPATH/zsh/" < <(curl -s -L 'https://git.io/fhkwE')
fi

source "${ITERM_INTEGRATION}"

if ! command_exists brew; then
  echo "Please install homebrew"
  return 0
fi

command_exists atuin || brew install atuin
command_exists eza || brew install eza

# # Define a list of commands to check and install
# commands_to_check=(
#   atuin
#   bat
#   btm
#   broot
#   dust
#   duf
#   eza
#   fd
#   procs
#   sd
#   starship
#   zoxide
# )

# # Loop through the list of commands
# for cmd in "${commands_to_check[@]}"; do
#   # Check if the command exists
#   if ! command_exists "$cmd" ; then
#     echo "$cmd not found. Installing Brewfile..."
#     # Install the command using brew
#     brewload 
#     break
#   fi
# done
