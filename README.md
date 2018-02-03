# thesaitama dotfile

## premise
 * macOS 10.13
 * bash 4.4
 * MacPorts

## how to install
Clone this repository
> git clone git@github.com:thesaitama/dotfiles.git ~/dotfiles

To run dotfileslins.sh
> sh ./dotfileslink.sh

## description
I will give you updates this repository.

### bash
 * .bash_profile
 * .bashrc

### emacs
 * .emacs.el
 * elisp/

### vim
 * .vimrc
 * .vim/

### tmux
 * .tmux.conf
 * tmux-status-line

paste board support

```bash
sudo port install tmux-pasteboard
```

### git
 * .gitconfig
 * .gitignore_global
 * .tigrc

install diff-hilight

```bash
cd /opt/local/share/git/contrib/diff-highlight/
sudo make
sudo ln -s /opt/local/share/git/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
```

### fzf
 * .fzfcmd.sh

install fzf

```bash
git clone https://github.com/junegunn/fzf.git ~/.fzf
cd ~/.fzf
./install
```

### etc

```bash
sudo port install readline
```
