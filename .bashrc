# thesaitama@ .bashrc

# Unicode Support
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

# Path
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:/usr/X11R6/bin:/Developer/Tools:$PATH
export MANPATH=/opt/local/man:$MANPATH

# Env
export CLICOLOR=1
export LSCOLORS=gxfxcxdxbxegedabagacad

# Alias
alias sgi="sgi64"
alias ls='ls -avlGF'

# /Applications Alias (Mac OSX)
alias syspref='open -a "System Preferences"'
alias chrome='open -a google\ chrome'
alias firefox='open -a firefox'
alias thunderbird='open -a thunderbird'
alias vsc='open -a visual\ studio\ code'
alias basecamp='open -a garmin\ basecamp'

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
git_branch() {
  echo $(git branch --no-color 2>/dev/null | sed -ne "s/^\* \(.*\)$/\1/p")
}
export PS1="\u@\h:\[\033[35m\]$(git_branch)\[\033[0m\]$ "

umask 022

set -o posix

# Make bash check it's window size after a process completes
shopt -s checkwinsize

# noclobber
set noblobber
