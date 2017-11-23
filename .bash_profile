
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bash_profile

# Editor
export EDITOR='emacsclient -nw'

# Unicode Support
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

# Path
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:/usr/X11R6/bin:/Developer/Tools:$PATH

# Path Python
export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-package:$PYTHONPATH

# NAOqi SDK
#export PYTHONPATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/python2.7/site-packages:$PYTHONPATH
#export DYLD_LIBRARY_PATH=/usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib:$DYLD_LIBRARY_PATH
#ln -s /usr/local/bin/naoqi/pynaoqi-python2.7-2.5.5.5-mac64/lib/* /usr/local/lib

# Env
export CLICOLOR=1
export LSCOLORS=gxfxcxdxbxegedabagacad

# for Fink
#test -d /sw && export PATH=/sw/bin:/sw/sbin:$PATH &&
#               export MANPATH=/sw/share/man:$MANPATH
#test -r /sw/bin/init.sh && . /sw/bin/init.sh

# for MacPorts
test -d /opt && export PATH=/opt/local/bin:/opt/local/sbin:$PATH &&
                export MANPATH=/opt/local/share/man:/opt/local/man:$MANPATH

# (If you'd like to use Qt3/X11 newer than Apr 24, 2006.)
#export QTDIR=/opt/local/lib/qt3

# (If you'd like to use Qt4/Mac.)
#export QTDIR=/usr/local/Trolltech/Qt-4.2.2
#export PATH=$QTDIR/bin:$PATH
#export QMAKESPEC=$QTDIR/mkspecs/macx-xcode
#export QMAKESPEC=$QTDIR/mkspecs/macx-g++

# (If you'd like to use CVS as CMS.)
#export CVS_RSH=ssh

# X-Window System
export DISPLAY=":0.0"
export LD_LIBRARY_PATH=/usr/X11R6/lib

# load .bashrc
if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

# load private settings
if [ -f ~/.bash_private ] ; then
  . ~/.bash_private
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

# iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
