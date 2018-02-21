
"   _   _                     _ _
"  | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
"  | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
"  | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
"   \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

"sudo port install vim +huge +python36

set shortmess+=I

set langmenu=en_US
let $LANG = 'en_US'

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

set showcmd

if has('mouse')
  set mouse=a
  if has('mouse_sgr')
    set ttymouse=sgr
  elseif v:version > 703 || v:version is 703 && has('patch632')
    set ttymouse=sgr
  else
    set ttymouse=xterm2
  endif
endif

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
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set nobackup
set nocursorline
set noerrorbells
set noignorecase
set nostartofline
set notitle
set nowritebackup
set nrformats-=octal
"set number
set paste
set ruler
set scrolloff=5
set shiftwidth=4
set showbreak=↪
set showmatch matchtime=1
set smartindent
set tabstop=4
set visualbell t_vb=
set whichwrap=b,s,[,],<,>
set wildmenu
set wrapscan

autocmd InsertEnter,InsertLeave * set cursorline!

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

nmap n nzz 
nmap N Nzz 
nmap * *zz 
nmap # #zz 
nmap g* g*zz 
nmap g# g#zz

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
" syntastics

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'passive' }

" ------------------------------------------------------------------------
" lightline

let g:lightline = {
        \ 'colorscheme': 'wombat',
        \ 'mode_map': {'c': 'NORMAL'},
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ],
        \   'right': [ [ 'syntastic', 'lineinfo' ],
        \              [ 'percent' ],
        \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
        \ },
        \ 'component_function': {
        \   'modified': 'LightlineModified',
        \   'readonly': 'LightlineReadonly',
        \   'fugitive': 'LightlineFugitive',
        \   'filename': 'LightlineFilename',
        \   'fileformat': 'LightlineFileformat',
        \   'filetype': 'LightlineFiletype',
        \   'fileencoding': 'LightlineFileencoding',
        \   'mode': 'LightlineMode'
        \ },
        \ 'component_expand': {
        \   'syntastic': 'SyntasticStatuslineFlag',
        \ },
        \ 'component_type': {
        \   'syntastic': 'error',
        \ }
        \ }

augroup AutoSyntastic
  autocmd!
  autocmd BufWritePost *.c,*.cpp call s:syntastic()
  autocmd BufWritePost *.py,*.php,*.xml call s:syntastic()
augroup END
function! s:syntastic()
  SyntasticCheck
  call lightline#update()
endfunction

function! LightlineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft == 'unite' ? unite#get_status_string() :
        \  &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
    return fugitive#head()
  else
    return ''
  endif
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

" ------------------------------------------------------------------------
" colorscheme

autocmd ColorScheme * highlight Comment ctermfg=2
autocmd ColorScheme * highlight Normal ctermbg=none
autocmd ColorScheme * highlight nonText ctermbg=none

colorscheme onedark
syntax enable

" ------------------------------------------------------------------------
" fzf

call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })
command! FZFMru call fzf#run({
\  'source':  v:oldfiles,
\  'sink':    'e',
\  'options': '-m -x +s',
\  'down':    '40%'})

nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>x :Commands<CR>
nnoremap <Leader>f :GFiles<CR>
nnoremap <Leader>a :Ag<CR>
nnoremap <Leader>k :bd<CR>
nnoremap <Leader>r :FZFMru<CR>

" ------------------------------------------------------------------------
" auto reload .vimrc

augroup source-vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC | set foldmethod=marker
  autocmd BufWritePost *gvimrc if has('gui_running') source $MYGVIMRC
augroup END

" ------------------------------------------------------------------------
