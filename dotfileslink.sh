#! /bin/bash

sudo -v

# bash
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.inputrc ~/.inputrc
ln -s ~/dotfiles/.fzfcmd.sh ~/.fzfcmd.sh

# editor
ln -s ~/dotfiles/.emacs.el ~/.emacs.el
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dotfiles/.vim ~/.vim

# neovim
ln -s ~/dotfiles/.vim ~/.config/nvim
ln -s ~/dotfiles/.vimrc ~/.config/nvim/init.vim

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

# etc
ln -s ~/dotfiles/.aspell.conf ~/.aspell.conf
ln -s ~/dotfiles/.fd2rc ~/.fd2rc

