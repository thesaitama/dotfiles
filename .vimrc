
set encoding=utf-8

source $VIMRUNTIME/delmenu.vim 
set langmenu=none 
source $VIMRUNTIME/menu.vim

if has("multi_lang")
  language C
endif

if has("syntax")
  syntax on
endif

set laststatus=2
set statusline=%F%m%r%h%w\ %{&ff}\ %Y\ \%02.2B\ %04l,%04v\ 
set statusline+=%{has('multi_byte')&&\&fileencoding!=''?&fileencoding:&encoding}

set background=dark

set showcmd
set mouse=a

"set number
set title
set ambiwidth=double
set tabstop=4
set expandtab
set shiftwidth=4
set smartindent
set ruler
set incsearch
set hlsearch
set ignorecase
set paste
set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set nrformats-=octal
set hidden
set history=49
set whichwrap=b,s,[,],<,>
set backspace=indent,eol,start
set wildmenu

set showmatch matchtime=1

set nowritebackup
set nobackup

" auto reload .vimrc
augroup source-vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC | set foldmethod=marker
  autocmd BufWritePost *gvimrc if has('gui_running') source $MYGVIMRC
augroup END
