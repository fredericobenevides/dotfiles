"-------------------------------------------------------------------------------
"  Delete trailing spaces before saving
"-------------------------------------------------------------------------------
autocmd BufWritePre * :%s/\s\+$//ge

"-------------------------------------------------------------------------------
"  Auto save
"-------------------------------------------------------------------------------
autocmd FocusLost * :wa

"-------------------------------------------------------------------------------
"  Settings for Filetype
"-------------------------------------------------------------------------------
autocmd FileType help wincmd J " Open help in bottom window

autocmd FileType go setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType go highlight SpecialKey ctermbg=bg guibg=bg

autocmd FileType java setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType java highlight SpecialKey ctermbg=bg guibg=bg

"-------------------------------------------------------------------------------
"  Disable flash error
"-------------------------------------------------------------------------------
autocmd VimEnter * set vb t_vb= " gvim resets it, so I set it up again

"-------------------------------------------------------------------------------
"  Go
"-------------------------------------------------------------------------------
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
"  NERDTree
"-------------------------------------------------------------------------------
autocmd VimEnter * call g:OpenNerdTree()

"-------------------------------------------------------------------------------
"  UltiSnips
"-------------------------------------------------------------------------------
autocmd InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"