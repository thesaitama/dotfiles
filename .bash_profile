
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bash_profile

# ------------------------------------------------------------------------
# Language and Unicode Support

export LESSCHARSET=utf-8
export LANG=ja_JP.UTF-8
# export LC_ALL=$LANG
# export LC_CTYPE=$LANG

# dotfiles bin
export PATH=~/dotfiles/bin:$PATH

# XDG Config
export XDG_CONFIG_HOME="${HOME}/.config"

# X-Window System
export DISPLAY=":0.0"
export PATH=/usr/X11R6/bin:$PATH
export LD_LIBRARY_PATH=/usr/X11R6/lib

# Editor
export EDITOR='emacsclient -nw'

# ------------------------------------------------------------------------
# Programing language

# Python PIP
export PIP_CONFIG_FILE="${HOME}/pip.conf"

# GTags
export GTAGSLABEL="pygments"

# GO
export GOPATH="${HOME}/go"
export PATH="${HOME}/go/bin:${PATH}"

# Rust
# >curl https://sh.rustup.rs -sSf | sh
#
# installer
export PATH="$HOME/.cargo/bin:$PATH"

# MacOS old Devloper Tools
test -d /Developer && export PATH=/Developer/Tools:$PATH

# ------------------------------------------------------------------------
# Package system (for Mac)

# Fink
# test -d /sw && export PATH=/sw/bin:/sw/sbin:$PATH && export MANPATH=/sw/share/man:$MANPATH
# test -r /sw/bin/init.sh && . /sw/bin/init.sh

# MacPorts
test -d /opt && export PATH=/opt/local/bin:/opt/local/sbin:$PATH &&
  export PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH &&
  export MANPATH=/opt/local/share/man:/opt/local/man:$MANPATH &&
  export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-package:$PYTHONPATH

# Homebrew
export PATH=/usr/local/bin:$PATH
export HOMEBREW_NO_EMOJI=1
#
# installer
# > /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Qt4/Mac
test -d /usr/local/Qt4.8 && export QTDIR=/usr/local/Qt4.8 &&
  export PATH=$QTDIR/bin:$PATH &&
  export QMAKESPEC=$QTDIR/mkspecs/macx-xcode &&
  export QMAKESPEC=$QTDIR/mkspecs/macx-g++

# MySQL
export PATH=/usr/local/mysql/bin:$PATH

# ------------------------------------------------------------------------
# NAOqi SDK

test -d /usr/local/bin/naoqi && export PYTHONPATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/python2.7/site-packages:$PYTHONPATH &&
  export DYLD_LIBRARY_PATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib:$DYLD_LIBRARY_PATH
#
# > ln -s /usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/* /usr/local/lib  

# ------------------------------------------------------------------------

# load .bashrc
if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

# load private settings
if [ -f ~/.bash_private ] ; then
  . ~/.bash_private
fi

# iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

