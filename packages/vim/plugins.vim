" CtrlP with Ag
let g:ctrlp_show_hidden=1" show hidden files
let g:ctrlp_user_command='ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_use_caching=0

" Easy-align
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Emmet
let g:user_emmet_leader_key='<c-e>'
let g:user_emmet_expandabbr_key='<c-e>'

" Git
map <leader>gb :Gbrowse<CR>
map <leader>gc :Gcommit<CR>
map <leader>gd :Gdiff<CR>
map <leader>gl :Glog<CR>
map <leader>gp :Gpush<CR>
map <leader>gs :Git push<CR>

" Grep with Ag
set grepprg=ag\ --nogroup\ --nocolor

" Gundo
nnoremap <F5> :GundoToggle<CR>

" NERDTree
map <c-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1" show hidden files

" RSpec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" Snippets -> Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_ruby_checkers = ['rubocop', 'mri']

" UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"

" YouCompleteMe
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]
