# thesaitama@ .bash_profile

if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

if [ -f ~/.bash_private ] ; then
  . ~/.bash_private
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

#git clone https://github.com/junegunn/fzf.git ~/.fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -f ~/.bash_private ] ; then
  . ~/.fzfcmd.sh
fi

# bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
  . /opt/local/etc/profile.d/bash_completion.sh
fi
