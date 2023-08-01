These are my dotfiles. Read about them [here](http://www.blaenkdenum.com/posts/dots/).

### Dependencies

There are no real, hard dependencies since these are simply configuration files, aside from perhaps the font that I use if you don't change it. That said, here are some programs I use for which these configuration files mainly exist:

- Font: DejaVu Sans Mono, `ttf-dejavu`
- Terminal: urxvt, AUR `rxvt-unicode-patched`
- Terminal Multiplexer: `tmux`
- Emacs 25
- vim + gvim: `gvim`
- Shell: `zsh`
- Awk: `gawk` on Ubuntu
- `wmctrl` for urxvt fullscreen
- Fuzzy Complete: `fzf`
- Searching: `rg`
- Command Not Found: `pkgfile` (Arch)
- `ls` colors: `colord`
- Clipboard integration: `xsel`

### Installation

```bash
$ git clone https://github.com/blaenk/dots.git ~/.dots
$ cd ~/.dots
$ dots install
```

### Usage

Run the sprinkle script to deploy. It shows a prompt on how (or whether) to apply the files. The prompts are answered by providing the first letter of each word, e.g. "backup" would be "b". If you want to apply the action to every remaining item, capitalize the letter.

```
$ ./dots install

  · sprinkling dots from /home/user/.dots!
  · help: backup, overwrite, remove, skip
          capitalize to apply to all remaining
```

Once the zsh configuration files are deployed, you can use the `dots` command from anywhere to both deploy the dotfiles with `dots install` as well as update the dotfiles with `dots get`.

```
λ ~/.dots (master)
» dots get

  · checking for updates since be6d115
  · updated to b234aef

  + b234aef this is one more test
  + 8a9b1ab this is a test for update command

```

#### zsh

You'll want to have zsh installed and setup for your user. Install it, then run the following command.

```bash
$ chsh -s $(which zsh)
```

Afterward, log out and log back in for the change to take effect. The first time you open a zsh shell, zplug (a zsh package manager) will download the packages I use.

#### Theme Variant

I use the [Solarized](http://ethanschoonover.com/solarized) color theme for everything. Run the `set-theme` command to properly configure your desired variant:

```bash
$ dots set-theme light

# or
$ dots set-theme dark
```

This also installs the `.Xresources` file so that URxvt uses the appropriate theme.

An environment variable `USE_SOLARIZED_DARK` is exposed which, if set, means that the Solarized Dark theme is being used. This is used in Xresources, vim, emacs, tmux, fzf, and other configurations in order to fine-tune colors for the currently-enabled theme.

#### Command Not Found

You can get functionality where, if you enter a command that's not installed on your system, zsh will tell you which package it _is_ available in, if it finds it in some package. This is available for Ubuntu and Archlinux.

On arch, you'll have to install `pkgfile` and then generate the metadata needed for this:

```bash
$ sudo pacman -S pkgfile
$ sudo pkgfile -u
$ abiword
abiword may be found in the following packages:
  extra/abiword 3.0.0-2 /usr/bin/abiword
```
