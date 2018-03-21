#! /bin/bash

sudo -v

# bash
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.inputrc ~/.inputrc
ln -s ~/dotfiles/.fzfcmd.sh ~/.fzfcmd.sh

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
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.gitignore_global ~/.gitignore_global
ln -s ~/dotfiles/.tigrc ~/.tigrc

# tmux
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
# ln -s ~/dotfiles/tmux-status-line /usr/local/bin/tmux-status-line
# ln -s ~/dotfiles/tmux-pane-border /usr/local/bin/tmux-pane-border
# rm /usr/local/bin/tmux-status-line
# rm /usr/local/bin/tmux-pane-border

# python
ln -s ~/dotfiles/pip.conf ~/pip.conf
ln -s ~/dotfiles/.flake8 ~/.flake8

# javascript
ln -s ~/dotfiles/.tern-config ~/.tern-config

# tags
ln -s ~/dotfiles/.globalrc ~/.globalrc

# etc
ln -s ~/dotfiles/.aspell.conf ~/.aspell.conf
ln -s ~/dotfiles/.fd2rc ~/.fd2rc

