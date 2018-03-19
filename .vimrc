" .vimrc --- thesaitama Vim configuration

"   _   _                     _ _
"  | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
"  | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
"  | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
"   \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

" ------------------------------------------------------------------------
" install

"sudo port install vim +huge +python36
"sudo pip-3.6 install neovim

if !1 | finish | endif

" ------------------------------------------------------------------------
" basic setttings

set shortmess+=I

set langmenu=en_US
let $LANG='en_US'

source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

set rtp+=~/.fzf
set clipboard+=unnamed

scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-boms,utf-8,cp932,euc-jp
set fileformats=unix,dos,mac

set laststatus=2
"set statusline=%F%m%r%h%w\ %{&ff}\ %Y\ \%02.2B\ %04l,%04v\
"set statusline+=%{has('multi_byte')&&\&fileencoding!=''?&fileencoding:&encoding}

if has('mouse')
  set mouse=a
  if !has('nvim')
    if has('mouse_sgr')
      set ttymouse=sgr
        elseif v:version > 703 || v:version is 703 && has('patch632')
        set ttymouse=sgr
      else
        set ttymouse=xterm2
    endif
  endif
endif

set showcmd
set number

set ambiwidth=double
set autoread
set backspace=indent,eol,start
set expandtab
set helplang=ja,en
set hidden
set history=49
set hlsearch
set ignorecase
set incsearch
set list
set listchars=tab:»-,eol:↲,extends:»,precedes:«,nbsp:%
set nobackup
set noerrorbells
set noignorecase
set nostartofline
set notitle
set nowritebackup
set nrformats-=octal
set ruler
set scrolloff=5
set softtabstop=0
set shiftwidth=2
set showbreak=↪
set showmatch matchtime=1
set smartindent
set smarttab
set tabstop=2
set visualbell t_vb=
set virtualedit=block
set whichwrap=b,s,[,],<,>
set wildmenu
set wrapscan
"set paste "this option should be disable

"set nocursorline
autocmd InsertEnter * set cursorline
autocmd InsertLeave * set nocursorline

" ------------------------------------------------------------------------
" python path

function! s:python_path(ver)
  let ver = a:ver == 2 ? "" : a:ver
  let paths = split(glob("/usr/local/Cellar/python".ver."/*/Frameworks/Python.framework/Versions/*/Python"), "\n")
  if len(paths) > 0
    return paths[-1]
  endif
endfunction
let $PYTHON3_DLL = s:python_path(3)

" ------------------------------------------------------------------------
" keymap

if ! has('gui_running')
 set ttimeoutlen=10
 augroup FastEscape
   autocmd!
   au InsertEnter * set timeoutlen=0
   au InsertLeave * set timeoutlen=1000
 augroup END
endif

let mapleader = ","
let maplocalleader = 'm'

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

nnoremap <silent>bp :bprevious<CR>
nnoremap <silent>bn :bnext<CR>
nnoremap <silent>bb :b#<CR>
nnoremap <leader>ev :e ~/dotfiles/.vimrc
nnoremap <leader>cv :e ~/dotfiles/vim.txt
inoremap jj <Esc>
nnoremap <ESC><ESC> :nohl<CR>

" ------------------------------------------------------------------------
" dein plugin

let s:dein_dir = expand('~/.vim/dein')
" dein.vim
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" if not exist dein.vim fetch from github
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  let g:rc_dir    = expand('~/.vim/rc')
  let s:toml      = g:rc_dir . '/dein.toml'
  let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

  call dein#load_toml(s:toml,      {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})

  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
endif

" ------------------------------------------------------------------------
" plugins

source ~/dotfiles/cnf-syntastics.vim
source ~/dotfiles/cnf-lightline.vim
source ~/dotfiles/cnf-vimfiler.vim
source ~/dotfiles/cnf-fzf.vim

" ------------------------------------------------------------------------
" colorscheme

"autocmd ColorScheme * highlight Comment ctermfg=2
autocmd ColorScheme * highlight Normal ctermbg=none
autocmd ColorScheme * highlight nonText ctermbg=none

set background=dark
colorscheme kalisi
syntax enable

" ------------------------------------------------------------------------
" auto reload .vimrc

augroup source-vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC | set foldmethod=marker
  autocmd BufWritePost *gvimrc if has('gui_running') source $MYGVIMRC
augroup END

" ------------------------------------------------------------------------
