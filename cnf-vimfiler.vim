
" ------------------------------------------------------------------------
" vimfiler

let g:vimfiler_as_default_explorer=1
let g:vimfiler_ignore_pattern='\(^\.\|\~$\|\.pyc$\|\.[oad]$\)'

nnoremap <C-f> :VimFilerCurrentDir<CR>
inoremap <C-f> <ESC>:VimFilerCurrentDir<CR>

nnoremap <C-x><C-f> :VimFiler -project<CR>
inoremap <C-x><C-f> <ESC>:VimFiler -project<CR>

nnoremap <C-x><C-@> :Unite -direction=botright -default-action=vimfiler directory_mru<CR>

