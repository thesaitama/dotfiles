# thesaitama dotfile

## Premise

 * macOS 10.13
 * bash 4.4
 * MacPorts

## How to install

Clone this repository

```bash
git clone https://github.com/thesaitama/dotfiles.git ~/dotfiles
```

To run dotfileslins.sh

```bash
sh ./dotfileslink.sh
```

## Description

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

#### Paste board support

```bash
sudo port install tmux-pasteboard
```

### git

 * .gitconfig
 * .gitignore_global
 * .tigrc

#### Install diff-hilight

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

* Readline, EditorConfig

```bash
sudo port install readline
sudo port install editorconfig-core-c
```

* Web

```bash
npm -g install js-beautify
```

* TypeScript

```bash
sudo npm install tslint
sudo npm install typescript
sudo npm install -g clausreinke/typescript-tools
```

* Ruby

```bash
gem install pry pry-doc method_source
gem install ruby-lint
```

* Tags

```bash
sudo port install ctags
pip-2.7 install pygments
sudo port install global
cp -p /opt/local/share/gtags/gtags.conf ~/.globalrc
sed -i -e "s/exuberant-ctags\.la/exuberant-ctags.so/g" ~/.globalrc
sed -i -e "s/pygments-parser\.la/pygments-parser.so/g" ~/.globalrc
```

* aspell

```bash
sudo port install aspell
sudo port install aspell-dict-en
```
