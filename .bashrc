# thesaitama@ .bashrc

umask 022

# alias
alias e='emacsclient -nw -a ""'
alias emacs='emacsclient -nw -a ""'
alias ls='ls -avlGF'
alias g='git'

alias snao='dns-sd -B _naoqi._tcp'
alias mdlk='dns-sd -q'
#dns-sd -B _nao._tcp

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

# color chars
c_red="\e[31m"
c_green="\e[32m"
c_yellow="\e[33m"
c_blue="\e[34m"
c_purple="\e[35m"
c_cyan="\e[36m"
c_reset="\e[00m"

# PS1
#export PS1="${c_purple}\u@:${c_reset}${c_cyan}\W:${c_reset}$(_ps1_result)$ "
export PS1="${c_reset}${c_green}\W/${c_blue} \
${c_yellow}\$(eval \"res=\$?\"; [[ \${res} -eq 0 ]] && \
echo -en \"${c_reset}\${res}\" || echo -en \"${_pr_fg_red}\${res}\")${c_yellow} \
${c_blue}\\\$${c_reset} "


# Visual Studio Code
if [ "$(uname)" == 'Darwin' ]; then
  vsc() {
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


