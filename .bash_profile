# thesaitama@ .bash_profile

source ~/.bashrc
source ~/.bash_private

test -r /sw/bin/init.sh && . /sw/bin/init.sh

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
