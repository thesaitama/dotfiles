#! /bin/bash

# bash
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.inputrc ~/.inputrc

ln -s ~/dotfiles/.colorrc ~/.colorrc

# emacs
ln -s ~/dotfiles/.emacs.el ~/.emacs.el

# vim
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dotfiles/.vim ~/.vim

# neovim
# ~/.config/nvim@ -> ~/dotfiles/.vim
# ~/.config/nvim/.vim@ -> ~/.vim
# ~/.config/nvim/init.vim@ -> ~/.vimrc
ln -s ~/.vim ~/.config/nvim
ln -s ~/.vimrc ~/.config/nvim/init.vim

# git
# ln -s ~/dotfiles/.gitconfig ~/.gitconfig
# ln -s ~/dotfiles/.gitignore_global ~/.gitignore_global
ln -s ~/dotfiles/.tigrc ~/.tigrc

# screen
ln -s ~/dotfiles/.screenrc ~/.screenrc

# tmux
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
# rm /usr/local/bin/tmux-status-line
# rm /usr/local/bin/tmux-pane-border

# python
ln -s ~/dotfiles/pip.conf ~/pip.conf
ln -s ~/dotfiles/.flake8 ~/.flake8

# java
ln -s ~/dotfiles/sbt ~/.sbt

# javascript
ln -s ~/dotfiles/.eslintrc ~/.eslintrc
ln -s ~/dotfiles/.tern-config ~/.tern-config

# tags
ln -s ~/dotfiles/.globalrc ~/.globalrc

# aspell
ln -s ~/dotfiles/.aspell.conf ~/.aspell.conf

# etc
ln -s ~/dotfiles/.fd2rc ~/.fd2rc

