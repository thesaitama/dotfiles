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
I will give you updates this repository

### bash
 * .bash_profile
 * .bashrc

### emacs
 * .emacs
 * .emacs.unuse

#### dependencies
 * auto-complete
 * rainbow-mode
 * rainbow-delimiters
 * php-mode
 * mmm-mode
 * python-mode
 * jedi
 * flycheck
 * helm
 * yasnippet
 * yasnippet-snippets
 * helm-c-yasnippet
 * magit
 * neotree

 run on emacs,
 type `M-x list-packages <dependencies>`

### vim
 * .vimrc
 * .vim/

### tmux
 * .tmux.conf
 * tmux-status-line

paste board support
```
sudo port install tmux-pasteboard
```

### git
 * .gitconfig
 * .gitignore_global
 * .tigrc

install diff-hilight
```
cd /opt/local/share/git/contrib/diff-highlight/
sudo make
sudo ln -s /opt/local/share/git/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
```

### fzf
 * .fzfcmd.sh

install fzf
```
git clone https://github.com/junegunn/fzf.git ~/.fzf
cd ~/.fzf
./install
```

