# highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern root)
ZSH_HIGHLIGHT_STYLES[precommand]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=cyan,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=cyan,underline'

# to underline
# tput smul; then tput rmul;

export LESS_TERMCAP_mb=$(tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)

# search
export LESS_TERMCAP_so=$(tput bold; tput setaf 11; tput setab 7) # black on gray (vim)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)

# underline start/end
export LESS_TERMCAP_us=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_ue=$(tput sgr0)

export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

export LESS="--RAW-CONTROL-CHARS"
