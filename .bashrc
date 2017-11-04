# thesaitama@ .bashrc

umask 022

# Alias
alias sgi="sgi64"
alias ls='ls -avlGF'

# /Applications Alias (Mac OSX)
alias syspref='open -a "System Preferences"'
alias chrome='open -a google\ chrome'
alias firefox='open -a firefox'
alias thunderbird='open -a thunderbird'
alias vsc='open -a visual\ studio\ code'

# Make bash check it's window size after a process completes
shopt -s checkwinsize

# noclobber
set noblobber

# tmux
# sudo port install tmux-pasteboard
# tmux ssh
ssh() {
  if [ "$(ps -p $(ps -p $$ -o ppid=) -o comm=)" = "tmux" ]; then
    tmux rename-window ${@: -1}
    command ssh "$@"
    tmux set-window-option automatic-rename "on" 1>/dev/null
  else
    command ssh "$@"
  fi
}

# fzf
# > git clone https://github.com/junegunn/fzf.git ~/.fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
if [ -f ~/.fzfcmd.sh ] ; then
  . ~/.fzfcmd.sh
fi

# bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
  . /opt/local/etc/profile.d/bash_completion.sh
fi
