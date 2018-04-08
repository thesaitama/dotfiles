
" ------------------------------------------------------------------------
" vimfiler

let g:vimfiler_as_default_explorer=1
let g:vimfiler_ignore_pattern='\(^\.\|\~$\|\.pyc$\|\.[oad]$\)'

nnoremap <C-x><C-f> :VimFilerCurrentDir<CR>
inoremap <C-x><C-f> <ESC>:VimFilerCurrentDir<CR>

nnoremap <C-x><C-r> :Unite -direction=botright -default-action=vimfiler directory_mru<CR>

