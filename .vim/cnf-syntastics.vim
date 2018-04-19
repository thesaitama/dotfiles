
" ------------------------------------------------------------------------
" syntastics

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'passive' }
let g:syntastic_javascript_checkers = ['eslint']

augroup AutoSyntastic
  autocmd!
  autocmd BufWritePost *.c,*.cpp call s:syntastic()
  autocmd BufWritePost *.js,*.py,*.php,*.rb call s:syntastic()
  autocmd BufWritePost *.html,*.xml call s:syntastic()
augroup END
function! s:syntastic()
  SyntasticCheck
  call lightline#update()
endfunction
