These are my dotfiles. Read about them [here](http://www.blaenkdenum.com/posts/dots/).

### Installation

``` bash
$ cd $HOME # or just cd
$ git clone https://github.com/blaenk/dots.git .dots
$ cd .dots
```

### Usage

Run the sprinkle script to deploy. It shows a prompt on how (or whether) to apply the files. The prompts are answered by providing the first letter of each word, e.g. "backup" would be "b". If you want to apply the action to every remaining item, capitalize the letter.

```
$ ./sprinkle

  · sprinkling dots from /home/user/.dots!
  · help: backup, overwrite, remove, skip
          capitalize to apply to all remaining
```

Once the zsh configuration files are deployed, you can run the `sprinkle` command from anywhere.

#### zsh

You'll want to have zsh installed and setup for your user. Install it, then run the following command.

``` bash
$ chsh -s $(which zsh)
```

Afterward, log out and log back in for the change to take effect. The first time you open a zsh shell, zsh antigen (a zsh package manager) will download the packages I use. Don't interrupt this process.

#### urxvt

If you use urxvt, you'll want to merge the `.Xresources` into your `xrdb` to use the Solarized Light theme that I use:

``` bash
$ xrdb -merge ~/.Xresources
```


