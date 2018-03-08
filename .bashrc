
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bashrc

umask 022

# alias
alias e='emacsclient -nw -a ""'
# alias e='TERM=xterm-256color-italic emacsclient -nw -a ""'
alias emacs='emacsclient -nw -a ""'
alias minimacs='\emacs -q -l ~/dotfiles/minimacs.el'
alias ls='ls -avhplGF'
alias g='git'
alias ..='cd ..'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

alias c='pygmentize -O style=monokai -f console256 -g'

alias snao='dns-sd -B _naoqi._tcp'
alias mdlk='dns-sd -q'
#dns-sd -B _nao._tcp

alias cleanupDS="find . -type f -name '*.DS_Store' -ls -delete"

# macOS Only
if [ "$(uname)" == 'Darwin' ]; then
  # /Applications Alias (Mac OSX)
  alias syspref='open -a "System Preferences"'
  alias reminders='open -a reminders'
  alias chrome='open -a google\ chrome'
  alias firefox='open -a firefox'
  # macOS Finder
  alias finderShowH='defaults write com.apple.finder ShowAllFiles TRUE'
  alias finderHideH='defaults write com.apple.finder ShowAllFiles FALSE'
fi

# Env (shell)
export CLICOLOR=1
export LSCOLORS=gxfxcxdxbxegedabagacad
export PROMPT_DIRTRIM=1
export HISTCONTROL=ignoredups:ignorespace:erasedups

# color man
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;32m") \
    LESS_TERMCAP_md=$(printf "\e[1;36m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[35m") \
    man "$@"
}

# color chars for PS1
c_red="\[\033[31m\]"
c_green="\[\033[32m\]"
c_yellow="\[\033[33m\]"
c_blue="\[\033[34m\]"
c_purple="\[\033[35m\]"
c_cyan="\[\033[36m\]"
c_reset="\[\033[00m\]"

# PS1
#export PS1="${c_purple}\u@:${c_reset}${c_cyan}\W:${c_reset}$(_ps1_result)$ "
export PS1="${c_reset}${c_green}\W/ \
${c_yellow}\$(eval \"res=\$?\"; [[ \${res} -eq 0 ]] && \
echo -en \"${c_reset}\${res}\" || echo -en \"${_pr_fg_red}\${res}\") \
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

set completion-ignore-case on
set bell-style none
set visible-stats on

# git-completion
if [ "$(uname)" == 'Darwin' ]; then
  source /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash
fi

# tmux
# lunch tmux
tm() {
  tmux ls > /dev/null
  if [ $? -eq 1 -a -z "$TMUX" ]; then
    exec tmux
  elif [ -z "$TMUX" ] ; then
    exec tmux attach
  else
    echo "sessions should be nested with care."
  fi
}

# run new pane
s(){
    if [ $# -eq 0 ]; then
        cat > /tmp/tmux.tmp && tmux split-window -v "less /tmp/tmux.tmp"
    else
        tmux split-window -v "$*"
    fi
}

# rename window-name when ssh
ssh() {
  if [ "$(ps -p $(ps -p $$ -o ppid=) -o comm=)" = "tmux" ]; then
    tmux rename-window ${@: -1}
    command ssh "$@"
    tmux set-window-option automatic-rename "on" 1>/dev/null
  else
    command ssh "$@"
  fi
}

# rename window-name when exit
exit() {
  if [ "$(ps -p $(ps -p $$ -o ppid=) -o comm=)" = "tmux" ]; then
    tmux rename-window ${@: -1}
    command exit
    tmux set-window-option automatic-rename "on" 1>/dev/null
  else
    command exit
  fi
}

# bash-completion
# > sudo port install bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
  . /opt/local/etc/profile.d/bash_completion.sh
fi

# autojump
# > sudo port install autojump
if [ -f /opt/local/etc/profile.d/autojump.sh ]; then
  . /opt/local/etc/profile.d/autojump.sh
fi

# fzf
# > git clone https://github.com/junegunn/fzf.git ~/.fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
if [ -f ~/.fzfcmd.sh ] ; then
  . ~/.fzfcmd.sh
  export FZF_DEFAULT_OPTS='--height 40% --reverse'
  export FZF_DEFAULT_COMMAND='ag -g ""'
  export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
  export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
fi

# enhancd
# > git clone https://github.com/b4b4r07/enhancd ~/.enhancd
if [ -f ~/.enhancd/init.sh ]; then
  export ENHANCD_FILTER=fzf
  source ~/.enhancd/init.sh
fi

# .inputrc
[ -f ~/.inputrc ] && bind -f ~/.inputrc


