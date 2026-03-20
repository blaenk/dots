# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A chezmoi-managed dotfiles repository. Files here are source templates/configs that chezmoi deploys to `~`. The working directory is `~/.local/share/chezmoi`.

## Chezmoi Conventions

- `dot_` prefix → `.` in target (e.g., `dot_tmux.conf` → `~/.tmux.conf`)
- `dot_config/` → `~/.config/`
- `private_` prefix → deployed with restricted permissions (e.g., `private_atuin`, `private_fish`)
- `executable_` prefix → deployed with execute bit set
- `.tmpl` suffix → Go template, rendered by chezmoi before deployment
- `.chezmoiignore` lists files tracked in git but NOT deployed (e.g., `Brewfile`, `readme.md`, `zsh/`, `iterm2/`)
- Apply changes: `chezmoi apply` (or `chezmoi apply ~/.tmux.conf` for a single file)
- Preview diff: `chezmoi diff`
- Edit source from target path: `chezmoi edit ~/.tmux.conf`

## Key Config Files

| Source | Target | Notes |
|--------|--------|-------|
| `dot_tmux.conf` | `~/.tmux.conf` | Extensive tmux config, vi-mode, 3-line status bar |
| `dot_zshrc` | `~/.zshrc` | Zinit plugin manager, lazy-loaded plugins |
| `dot_zshenv` | `~/.zshenv` | Environment variables, PATH setup |
| `dot_gitconfig.tmpl` | `~/.gitconfig` | Template: OS-conditional SSH signing (1Password on macOS) |
| `dot_vimrc` | `~/.vimrc` | Vim configuration |
| `dot_config/starship.toml` | `~/.config/starship.toml` | Starship prompt with One Dark palette |
| `dot_config/ghostty/config` | `~/.config/ghostty/config` | Ghostty terminal config |
| `dot_claude/settings.json` | `~/.claude/settings.json` | Claude Code hooks, plugins, permissions |

## Zsh Structure

`dot_zshrc` sources modular files from `zsh/` (which is in `.chezmoiignore` — it's a helper dir, not deployed directly). These files are sourced by `dot_zshrc` via `$ZDOTDIR/zsh/*.zsh` pattern:

- `zsh/alias.zsh` — shell aliases
- `zsh/functions.zsh` — utility functions (worktree management, plugin updates)
- `zsh/path.zsh` — PATH/MANPATH/INFOPATH
- `zsh/zle.zsh` — zsh line editor widgets and keybindings
- `zsh/fzf.zsh` — fzf integration with tmux
- `zsh/completions.zsh` — fpath and completion setup
- `zsh/macos.zsh` / `zsh/linux.zsh` — OS-specific config
- `zsh/highlight.zsh` — syntax highlighting rules

## tmux Structure

`dot_tmux.conf` is the main config. Custom scripts live in `dot_tmux/scripts/`:

- `claude-attention.sh` — tracks Claude Code session attention state in tmux status bar
- `pinned.sh` — pinned window management
- `click.sh`, `help.sh`, `notify.sh` — UI helpers

### tmux Style Convention

In tmux `#[...]` inline format tags, use **spaces** to separate attributes, NOT commas:
```
#[fg=default bold]    ← correct
#[fg=default,bold]    ← wrong in inline tags
```

Commas ARE valid in `setw -g ... style` and `set -g ... style` options:
```
setw -g window-status-current-style fg=default,bold    ← correct
```

## After Making Changes

After editing any files in this repo, ask whether to apply them. If approved, run:

```bash
chezmoi apply && tmux source-file ~/.tmux.conf
```

## Template Conditionals

`dot_gitconfig.tmpl` uses `{{ if eq .chezmoi.os "darwin" }}` for OS-specific blocks. When editing `.tmpl` files, preserve Go template syntax and test with `chezmoi cat <target-path>`.
