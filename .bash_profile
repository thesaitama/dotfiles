# thesaitama@ .bash_profile

# Unicode Support
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

# Path
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:/usr/X11R6/bin:/Developer/Tools:$PATH

export MANPATH=/opt/local/man:$MANPATH

# Path Python
export PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:$PATH
export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-package:$PYTHONPATH

# Env
export CLICOLOR=1
export LSCOLORS=gxfxcxdxbxegedabagacad

# for Fink
#test -d /sw && export PATH=/sw/bin:/sw/sbin:$PATH &&
#               export MANPATH=/sw/share/man:$MANPATH
#test -r /sw/bin/init.sh && . /sw/bin/init.sh

# for MacPorts
test -d /opt && export PATH=/opt/local/bin:/opt/local/sbin:$PATH &&
                export MANPATH=/opt/local/share/man:$MANPATH

# (If you'd like to use Qt3/X11 newer than Apr 24, 2006.)
#export QTDIR=/opt/local/lib/qt3

# (If you'd like to use Qt4/Mac.)
#export QTDIR=/usr/local/Trolltech/Qt-4.2.2
#export PATH=$QTDIR/bin:$PATH
#export QMAKESPEC=$QTDIR/mkspecs/macx-xcode
#export QMAKESPEC=$QTDIR/mkspecs/macx-g++

# (If you'd like to use CVS as CMS.)
export CVS_RSH=ssh
# export CVSROOT=...

# X-Window System
export DISPLAY=":0.0"
export LD_LIBRARY_PATH=/usr/X11R6/lib

# PS
export PS1="\[\033[35m\]\u@\h:\[\033[0m\]\[\033[36m\]\W\[\033[0m\]$ "

# bashrc
if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

# private settings
if [ -f ~/.bash_private ] ; then
  . ~/.bash_private
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

# iTerm shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


