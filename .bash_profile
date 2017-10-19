# thesaitama@ .bash_profile

if [ -f ~/.bashrc ] ; then
. ~/.bashrc
fi

if [ -f ~/.bash_private ] ; then
. ~/.bash_private
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
