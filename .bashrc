# thesaitama@ .bashrc

umask 022

# alias
alias ls='ls -avlGF'

# /Applications Alias (Mac OSX)
if [ "$(uname)" == 'Darwin' ]; then
  alias syspref='open -a "System Preferences"'
  alias reminders='open -a reminders'
  alias chrome='open -a google\ chrome'
  alias firefox='open -a firefox'
  alias thunderbird='open -a thunderbird'
  alias excel='open -a microsoft\ excel'
  alias msword='open -a microsoft\ word'
  alias powerpoint='open -a microsoft\ powerpoint'
fi

# Visual Studio Code
if [ "$(uname)" == 'Darwin' ]; then
  vscode() {
    if [[ $# = 0 ]]
    then
      open -a "visual studio code"
    else
      [[ $1 = /* ]] && F="$1" || F="$PWD/${1#./}"
      open -a "visual studio code" --args "$F"
    fi
  }
fi

# Make bash check it's window size after a process completes
shopt -s checkwinsize

# noblobber (disable overwirte)
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

# .inputrc
[ -f ~/.inputrc ] && bind -f ~/.inputrc


