"-------------------------------------------------------------------------------
"  CtrlP Settings with Ag
"-------------------------------------------------------------------------------

let g:ctrlp_show_hidden = 1 " show hidden files
let g:ctrlp_use_caching = 0 " ag is fast, no need cache
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

"-------------------------------------------------------------------------------
"  Easy-align Settings
"-------------------------------------------------------------------------------
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"-------------------------------------------------------------------------------
"  Emmet Settings
"-------------------------------------------------------------------------------
let g:user_emmet_leader_key='<c-e>'
let g:user_emmet_expandabbr_key='<c-e>'

"-------------------------------------------------------------------------------
"  Git Mapping on Fugitive
"-------------------------------------------------------------------------------
map <leader>gb :Gblame<cr>
map <leader>gB :Gbrowse<cr>
map <leader>gc :Gcommit<cr>
map <leader>gd :Gvdiff<cr>
map <leader>ge :Gedit<cr>
map <leader>gl :Glog<cr>
map <leader>gp :Gpush<cr>
map <leader>gP :Gpull<cr>
map <leader>gr :Gread<cr>
map <leader>gs :Gstatus<cr>
map <leader>gw :Gwrite<cr>

"-------------------------------------------------------------------------------
"  Grep Settings
"-------------------------------------------------------------------------------
set grepprg=ag\ --nogroup\ --nocolor

"-------------------------------------------------------------------------------
"  Gundo Settings
"-------------------------------------------------------------------------------
nnoremap <F5> :GundoToggle<cr>

"-------------------------------------------------------------------------------
"  NERDTree Settings
"-------------------------------------------------------------------------------
map <c-n> :NERDTreeToggle<cr>
nmap <leader>nf :NERDTreeFind<cr>
let NERDTreeShowHidden = 1 " show hidden files
let NERDTreeQuitOnOpen = 1 " close after opening a file

"-------------------------------------------------------------------------------
"  RSpec.vim mappings
"-------------------------------------------------------------------------------
map <Leader>t :call RunCurrentSpecFile()<cr>
map <Leader>s :call RunNearestSpec()<cr>
map <Leader>l :call RunLastSpec()<cr>
map <Leader>a :call RunAllSpecs()<cr>

"-------------------------------------------------------------------------------
"  Syntastic Settings
"-------------------------------------------------------------------------------
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_ruby_checkers = ['rubocop', 'mri']

"-------------------------------------------------------------------------------
"  UltiSnips Settings
"-------------------------------------------------------------------------------
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"

"-------------------------------------------------------------------------------
"  YouCompleteMe Settings
"-------------------------------------------------------------------------------
let g:ycm_key_list_select_completion = []
let g:ycm_key_list_previous_completion = []
