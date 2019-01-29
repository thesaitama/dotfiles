# .bash_profile
#  _   _                     _ _
# | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
# | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
# | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
#  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

# thesaitama@ .bash_profile

# ------------------------------------------------------------------------
# Language and Unicode Support

# export LANG=ja_JP.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=$LANG
export LC_CTYPE=$LANG
# > sudo dpkg-reconfigure locales

export LESSCHARSET=utf-8

# dotfiles bin
export PATH=~/dotfiles/bin:$PATH

# XDG Config
export XDG_CONFIG_HOME="${HOME}/.config"

# X-Window System
export DISPLAY=":0.0"
test -d /usr/X11R6 && export PATH=/usr/X11R6/bin:$PATH &&
  export LD_LIBRARY_PATH=/usr/X11R6/lib

# Editor (Emacs)
export EDITOR="vi"
export EMACS_TRUE_COLOR_SEPARATOR=';'

# ------------------------------------------------------------------------
# Programing languages

export PYTHONIOENCODING=utf-8
export PIP_CONFIG_FILE="${HOME}/pip.conf"

# GTAGS
export GTAGSLABEL="pygments"

# GO
test -d ~/.go && export GOPATH="${HOME}/.go" &&
  export PATH="${HOME}/.go/bin:${PATH}"

# Rust
# install
# > curl https://sh.rustup.rs -sSf | sh
# > cargo install rustfmt racer ripgrep fd-find
# > rustup component add rust-src
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"

export PKGPATH=/usr/local/bin/

# opam configuration
test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# ------------------------------------------------------------------------
# Windows

function wincmd() {
  CMD=$1
  shift
  $CMD $* 2>&1 | iconv -f cp932 -t utf-8
}

# ------------------------------------------------------------------------

# load .bashrc
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# load private settings
if [ -f ~/.bash_private ]; then
  . ~/.bash_private
fi

# ------------------------------------------------------------------------
