#! /bin/bash

sudo -v

# bash
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.inputrc ~/.inputrc
ln -s ~/dotfiles/.fzfcmd.sh ~/.fzfcmd.sh

# editor
ln -s ~/dotfiles/.emacs ~/.emacs
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dotfiles/.vim ~/.vim

# git
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.gitignore_global ~/.gitignore_global
ln -s ~/dotfiles/.tigrc ~/.tigrc

# tmux
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/tmux-status-line /usr/local/bin/tmux-status-line

# python
ln -s ~/dotfiles/pip.conf ~/pip.conf

# tags
ln -s ~/dotfiles/.globalrc ~/.globalrc
