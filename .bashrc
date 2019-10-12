# .bashrc
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bashrc

# Last Update: 2019-10-12 20:11:10

# ------------------------------------------------------------------------
# Env (shell)

umask 022

# export TERM=xterm-256color-italic
export TERMINFO=~/.terminfo
export CLICOLOR=1
export LSCOLORS=gxfxcxdxbxegedabagacad
export PROMPT_DIRTRIM=1

export LESS='-g -i -M -R -S -W -z-4 -x4'
export LESSOPEN='|lessfilter %s'
export PAGER=less

# bash options
shopt -s checkwinsize

# ------------------------------------------------------------------------
# Alias

alias sshx="TERM=xterm-256color ssh"

alias e='emacsclient -nw -a ""'
alias e256='TERM=screen-256color emacsclient -nw -a ""'
alias emacs='emacsclient -nw -a ""'
alias minimacs='\emacs -q -l ~/dotfiles/elisp/init-minimacs.el'
alias ekill='emacsclient -e "(kill-emacs)"'

alias ls='ls -avhpl --color'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

alias g='git'
alias ..='cd ..'

alias j='jobs -l'

alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'

alias c='pygmentize -O encoding=utf-8 -O style=monokai -f terminal256 -g'
cl() {
  c $1 | nl -n ln -b a
}
alias cl=cl

alias cleanupDS="find . -type f -name '*.DS_Store' -ls -delete"

# ------------------------------------------------------------------------
# Emacs integration

# https://masutaka.net/chalow/2011-09-28-1.html

## Invoke the ``dired'' of current working directory in Emacs buffer.
function dired () {
  emacsclient -nw -e "(dired \"$PWD\")"
}

## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde () {
  EMACS_CWD=`emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/'`
  echo "chdir to $EMACS_CWD"
  cd "$EMACS_CWD"
}

# ------------------------------------------------------------------------
# History

prompt_dispatch() {
  export EXIT_STATUS="$?"
  local f
  for f in ${!PROMPT_COMMAND_*}; do
    eval "${!f}"
  done
  unset f
}
export PROMPT_COMMAND="prompt_dispatch"
export PROMPT_COMMAND_HISTSAVE="share_history"

# share history
share_history() {
  history -a
  history -c
  history -r
}
shopt -u histappend

export HISTSIZE=10000
export HISTCONTROL=ignoredups:ignorespace:erasedups
export HISTIGNORE="fg*:bg*:history*"
export HISTTIMEFORMAT='%Y%m%d %T ';

# ------------------------------------------------------------------------
# Color

# man
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
c_gray="\[\033[37m\]"
c_reset="\[\033[00m\]"

# PS1
if [ "${SSH_CONNECTION}" ] ; then
  export PS1="${c_reset}${c_green}\u@\h: ${c_reset}${c_cyan}\W/ \
${c_yellow}\$(eval \"res=\$?\"; [[ \${res} -eq 0 ]] && \
echo -en \"${c_reset}\${res}\" || echo -en \"${_pr_fg_red}\${res}\") \
${c_blue}\\\$${c_reset} "
else
  export PS1="${c_reset}${c_green}\W/ \
${c_yellow}\$(eval \"res=\$?\"; [[ \${res} -eq 0 ]] && \
echo -en \"${c_reset}\${res}\" || echo -en \"${_pr_fg_red}\${res}\") \
${c_blue}\\\$${c_reset} "
fi

# ------------------------------------------------------------------------
# tmux

# lunch tmux
tm() {
  tmux ls > /dev/null
  if [ $? -eq 1 -a -z "$TMUX" ]; then
    tmux
  elif [ -z "$TMUX" ]; then
    target=$(tmux list-sessions |
               fzf -0 --no-sort --tac +m --exit-0 |
               perl -pe 's/^([0-9]+).+$/$1/g;')
    if [ -n "$target" ]; then
      tmux a -t ${target}
    else
      tmux new-session
    fi
  else
    echo "prevent nested TMUX sessions. :-P"
  fi
}

# kill tmux session
tk() {
  if [ -z $1 ]; then
    target=$(tmux list-sessions |
               fzf -1 -0 --no-sort --tac +m --exit-0 |
               perl -pe 's/^([0-9]+).+$/$1/g;')
    if [ -n "$target" ]; then
      tmux kill-session -t ${target}
    fi
  else
    tmux kill-session -t $1
  fi
}

# tmux precmd
precmd() {
  if [ ! -z $TMUX ]; then
    tmux refresh-client -S
  fi
}

# run new pane
tnp() {
  if [ $# -eq 0 ]; then
    cat > /tmp/tmux.tmp && tmux split-window -v "less /tmp/tmux.tmp"
  else
    tmux split-window -v "$*"
  fi
}

# rename window-name when ssh
ssh() {
  if [ "$(ps -p $(ps -p $$ -o ppid=) -o comm=)" = "tmux" ]; then
    local window_name=$(tmux display -p '#{window_name}')
    tmux rename-window ${@: -1}
    command ssh "$@"
    tmux rename-window ${window_name}
    tmux set-window-option automatic-rename "on" 1>/dev/null
  else
    command ssh "$@"
  fi
}

# ------------------------------------------------------------------------
# w3m for google, and wikitionary
# > sudo port install w3m

google() {
  local str opt
  if [ $# != 0 ]; then
    for i in $*; do
      str="$str+$i"
    done
    str=`echo $str | sed 's/^\+//'`
    opt='search?num=50&hl=ja&lr=lang_ja'
    opt="${opt}&q=${str}"
  fi
  w3m http://www.google.co.jp/$opt
}

wikitionary() {
  local str opt
  if [ $# != 0 ]; then
    for i in $*; do
      str="$str+$i"
    done
    str=`echo $str | sed 's/^\+//'`
    opt="${str}"
  fi
  w3m https://en.wiktionary.org/wiki/$opt
}

weblio() {
  local str opt
  if [ $# != 0 ]; then
    for i in $*; do
      str="$str+$i"
    done
    str=`echo $str | sed 's/^\+//'`
    opt="${str}"
  fi
  w3m https://ejje.weblio.jp/content/$opt
}

# ------------------------------------------------------------------------
# color-test

colortest_256() {
  for i in {0..255} ; do
    printf "\x1b[48;5;%sm%3d\e[0m " "$i" "$i"
    if (( i == 15 )) || (( i > 15 )) && (( (i - 15) % 12 == 0 )); then
      printf "\n";
    fi
  done
}

colortest_tc() {
  ~/dotfiles/terminfo/true_color_test.sh
}

# ------------------------------------------------------------------------
# bash-completion
# > sudo port install bash-completion
# path: /opt/local/etc/bash_completion.d
# add: '+bash_completion' >> /opt/local/etc/macports/variants.conf

if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
  . /opt/local/etc/profile.d/bash_completion.sh
fi

# ------------------------------------------------------------------------
# fasd
# > sudo port install fasd
# or
# > mkdir ~/src/
# > git clone https://github.com/clvv/fasd.git
# > cd fasd
# > sudo make install

if type fasd >/dev/null 2>&1; then
  eval "$(fasd --init auto)"
fi

# ------------------------------------------------------------------------
# fzf
# > git clone https://github.com/junegunn/fzf.git ~/.fzf
# cd ~/.fzf
# ./install

if [ -f ~/.fzf.bash ]; then
  if [ "$TERM" != 'emacs' ]; then
    source ~/.fzf.bash
    if [ "$PLATFORM" != "ming" ]; then
      export FZF_DEFAULT_OPTS='--height 40% --reverse'
    fi
    export FZF_DEFAULT_COMMAND='ag -g ""'
    export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
    export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
    # fzf extra commands
    if [ -f ~/dotfiles/fzf-commands.sh ]; then
      . ~/dotfiles/fzf-commands.sh
    fi
    # bash cd completion workaround
    complete -o bashdefault -o default -F _fzf_dir_completion cd
  fi
fi

# ------------------------------------------------------------------------
# enhancd
# > git clone https://github.com/b4b4r07/enhancd ~/.enhancd

if [ -f ~/.enhancd/init.sh ]; then
  export ENHANCD_FILTER=fzf
  source ~/.enhancd/init.sh
fi

# ------------------------------------------------------------------------
# ranger
# > sudo port install ranger

ranger() {
  [ -n "$RANGER_LEVEL" ] && exit || LESS="$LESS -+F -+X" command ranger "$@";
}
[ -n "$RANGER_LEVEL" ] && PS1="RANGER> $PS1"

if type ranger >/dev/null 2>&1; then
  alias rng='ranger'
fi

# ------------------------------------------------------------------------
# .inputrc

[ -f ~/.inputrc ] && bind -f ~/.inputrc

# ------------------------------------------------------------------------
