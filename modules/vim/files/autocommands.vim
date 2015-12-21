"-------------------------------------------------------------------------------
"  Delete trailing spaces before saving
"-------------------------------------------------------------------------------
autocmd BufWritePre * :%s/\s\+$//ge

"-------------------------------------------------------------------------------
"  Auto save and Auto Reload
"-------------------------------------------------------------------------------
autocmd FocusGained, BufEnter * :silent! !
autocmd FocusLost, WinLeave * :silent! w

"-------------------------------------------------------------------------------
"  Disable flash error
"-------------------------------------------------------------------------------
autocmd VimEnter * set vb t_vb= " gvim resets it, so I set it up again

"-------------------------------------------------------------------------------
"  FileType for Go
"-------------------------------------------------------------------------------
autocmd FileType go setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType go highlight SpecialKey ctermbg=bg guibg=bg

autocmd FileType go nmap <leader>r <Plug>(go-run)
autocmd FileType go nmap <leader>b <Plug>(go-build)
autocmd FileType go nmap <leader>t <Plug>(go-test)
autocmd FileType go nmap <leader>c <Plug>(go-coverage)
autocmd FileType go nmap <Leader>I <Plug>(go-implements)
autocmd FileType go nmap <Leader>i <Plug>(go-info)
autocmd FileType go nmap <Leader>R <Plug>(go-rename)

" Open documentation
autocmd FileType go nmap <Leader>dg <Plug>(go-doc)
autocmd FileType go nmap <Leader>dv <Plug>(go-doc-vertical)
autocmd FileType go nmap <Leader>db <Plug>(go-doc-browser)

"-------------------------------------------------------------------------------
"  Filetype for Help
"-------------------------------------------------------------------------------
autocmd FileType help wincmd J " Open help in bottom window

"-------------------------------------------------------------------------------
"  Filetype for Java
"-------------------------------------------------------------------------------
autocmd FileType java setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType java highlight SpecialKey ctermbg=bg guibg=bg

"-------------------------------------------------------------------------------
"  NERDTree
"-------------------------------------------------------------------------------
autocmd VimEnter * call g:OpenNerdTree()

"-------------------------------------------------------------------------------
"  UltiSnips
"-------------------------------------------------------------------------------
autocmd InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"

"-------------------------------------------------------------------------------
"  vim-indent-guides
"-------------------------------------------------------------------------------
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=234 ctermbg=234
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=235 ctermbg=235
