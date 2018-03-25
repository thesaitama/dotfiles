# .bash_profile
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bash_profile

# ------------------------------------------------------------------------
# Language and Unicode Support

export LESSCHARSET=utf-8
export LANG=C.UTF-8
# export LANG=ja_JP.UTF-8
# export LC_ALL=$LANG
# export LC_CTYPE=$LANG

# dotfiles bin
export PATH=~/dotfiles/bin:$PATH

# XDG Config
export XDG_CONFIG_HOME="${HOME}/.config"

# X-Window System
export DISPLAY=":0.0"
test -d /usr/X11R6 && export PATH=/usr/X11R6/bin:$PATH &&
  export LD_LIBRARY_PATH=/usr/X11R6/lib

# Editor
export EDITOR='emacsclient -nw'

# ------------------------------------------------------------------------
# Programing languages

# Python
pypath() {
  if [[ $# = 0 ]]
  then
    export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packagen
  else
    export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/$1/lib/python$1/site-packagen
  fi
}

export PYTHONIOENCODING=utf-8
export PIP_CONFIG_FILE="${HOME}/pip.conf"

# GTAGS
export GTAGSLABEL="pygments"

# GO
test -d ~/go && export GOPATH="${HOME}/go" &&
  export PATH="${HOME}/go/bin:${PATH}"

# Rust
# install
# > curl https://sh.rustup.rs -sSf | sh
# > cargo install rustfmt
# > cargo install racer
# > cargo install ripgrep fd-find
# > rustup component add rust-src
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"

export PKGPATH=/usr/local/bin/

# ------------------------------------------------------------------------
# MacOSX old Devloper Tools
# > xcode-select --install

test -d /Developer && export PATH=/Developer/Tools:$PATH

# ------------------------------------------------------------------------
# Fink

test -d /sw && export PATH=/sw/bin:/sw/sbin:$PATH && export MANPATH=/sw/share/man:$MANPATH
test -r /sw/bin/init.sh && . /sw/bin/init.sh

# ------------------------------------------------------------------------
# MacPorts

test -d /opt/local && export PATH=/opt/local/bin:/opt/local/sbin:$PATH &&
  export PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH &&
  export MANPATH=/opt/local/share/man:/opt/local/man:$MANPATH &&
  export NODE_PATH=/opt/local/lib/node_modules
  export PKGPATH=/opt/local/bin/ &&
  pypath

# useful command
# > sudo port uninstall inactive
# > sudo port -u upgrade outdated

# ------------------------------------------------------------------------
# Homebrew
# > /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# > brew tap caskroom/cask

export PATH=/usr/local/bin:$PATH
export HOMEBREW_NO_EMOJI=1

# ------------------------------------------------------------------------
# Mac special settings

# Qt4
test -d /usr/local/Qt4.8 && export QTDIR=/usr/local/Qt4.8 &&
  export PATH=$QTDIR/bin:$PATH &&
  export QMAKESPEC=$QTDIR/mkspecs/macx-xcode &&
  export QMAKESPEC=$QTDIR/mkspecs/macx-g++

# MySQL
test -d /usr/local/mysql && export export PATH=/usr/local/mysql/bin:$PATH

# iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# ------------------------------------------------------------------------
# NAOqi SDK

# test -d /usr/local/bin/naoqi && export PYTHONPATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/python2.7/site-packages:$PYTHONPATH &&
#   export DYLD_LIBRARY_PATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib:$DYLD_LIBRARY_PATH
#
# > ln -s /usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/* /usr/local/lib

alias snao='dns-sd -B _naoqi._tcp'
alias mdlk='dns-sd -q'
# dns-sd -B _nao._tcp

# ------------------------------------------------------------------------

# load .bashrc
if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

# load private settings
if [ -f ~/.bash_private ] ; then
  . ~/.bash_private
fi

# ------------------------------------------------------------------------
