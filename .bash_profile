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
# platform check
# https://qiita.com/b4b4r07/items/09815eda8ef72e0b472e

# ostype returns the lowercase OS name
ostype() {
  uname | tr "[:upper:]" "[:lower:]"
}

# os_detect export the PLATFORM variable as you see fit
os_detect() {
  case "$(ostype)" in
    *'linux'*)  PLATFORM='linux'   ;;
    *'darwin'*) PLATFORM='osx'     ;;
    *'bsd'*)    PLATFORM='bsd'     ;;
    *'ming'*)   PLATFORM='ming'    ;;
    *)          PLATFORM='unknown' ;;
  esac
  export PLATFORM
}

# is_osx returns true if running OS is Macintosh
is_osx() {
  os_detect
  if [ "$PLATFORM" = "osx" ]; then
    return 0
  else
    return 1
  fi
}

os_detect

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

# load .bashrc
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

if [ "$PLATFORM" = "osx" ]; then
  if [ -f ~/dotfiles/.bashrc_osx ]; then
    . ~/dotfiles/.bashrc_osx
  fi
fi

if [ "$PLATFORM" = "ming" ]; then
  if [ -f ~/dotfiles/.bashrc_ming ]; then
    . ~/dotfiles/.bashrc_ming
  fi
fi

# Linux Only
if [ "$PLATFORM" = "linux" ]; then
  eval `dircolors ~/.colorrc`
fi

# load private settings
if [ -f ~/.bash_private ]; then
  . ~/.bash_private
fi

# ------------------------------------------------------------------------
