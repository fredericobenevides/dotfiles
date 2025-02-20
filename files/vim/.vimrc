if has('nvim')
  call plug#begin('~/.local/share/nvim/plugged')
else
  call plug#begin('~/.vim/plugged')
endif

" Colors and themes {{{2
Plug 'joshdick/onedark.vim'
Plug 'bling/vim-airline'
" }}}2

" Dart {{{2
Plug 'dart-lang/dart-vim-plugin'
" }}}2

" Clojure {{{2
Plug 'Olical/conjure', {'tag': 'v4.21.0'}
Plug 'clojure-vim/vim-jack-in'
" }}}2

" Editor Utilities {{{2
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }
Plug 'majutsushi/tagbar'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'Yggdroot/indentLine'
Plug 'terryma/vim-multiple-cursors'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'https://github.com/honza/vim-snippets'
Plug 'dsznajder/vscode-es7-javascript-react-snippets', { 'do': 'yarn install --frozen-lockfile && yarn compile' }
Plug 'kien/rainbow_parentheses.vim'
Plug 'guns/vim-sexp'
Plug 'fredericobenevides/line-mover.vim'
" }}}2

" FZF {{{2}}}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': '~/.dotfiles/files/fzf/install.sh' }
Plug 'junegunn/fzf.vim'
" }}}2

" Git {{{2
Plug 'tpope/vim-fugitive'
Plug 'mattn/webapi-vim' | Plug 'mattn/gist-vim'
Plug 'junegunn/gv.vim'
" }}}2

" Html & css {{{2
Plug 'KabbAmine/vCoolor.vim'
" }}}2

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
" }}}2

" Tpope Utilities {{{2
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

if has('nvim')
  Plug 'radenling/vim-dispatch-neovim'
endif
" }}}2

" terminal integration {{{2
Plug 'voldikss/vim-floaterm'
" }}}2

call plug#end()
" }}}1

" Color and themes configs {{{1
" syntax on
syntax on
set t_Co=256
colorscheme onedark
set termguicolors " enable true colors in terminal

colorscheme dracula_pro
let g:dracula_colorterm = 0
" }}}1

" Config settings {{{1

" Disable netrw {{{2
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
" }}}2

" Backup Settings {{{2
set nobackup
set nowritebackup
set noswapfile
" }}}2

" Buffer and Edit Settings {{{2
set autoread " Automatically read a file that has changed on disk
set backspace=indent,eol,start " backspace working like other apps
set cmdheight=2 " Give more space for displaying messages.
set hidden " Allow unsaved buffers to be put in background
set cursorline
set completeopt-=preview " stop showing the preview window using YCM
set cpoptions+=$ " put $ at the end of the changed word
let loaded_matchparen = 1 " disable the match for parentheses
set nrformats= " treat all numerals as decimal
set shortmess+=c " Don't pass messages to |ins-completion-menu|
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" copy default to clipboard
if has('unnamedplus')
  set clipboard=unnamedplus
else
  set clipboard=unnamed
endif
" }}}2

" Enconding Settings {{{2
set encoding=utf-8
set fileencoding=utf-8
" }}}2

" Fold Settings {{{2
"set foldmethod=syntax
"set foldlevelstart=1
" }}}2

" History and Undo Settings {{{2
set history=1000
set undolevels=100
" }}}2

" Indent and Tab Settings {{{2
set autoindent " Copy indent from current line when starting a new line
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
" }}}2

" List Settings {{{2
set list
set listchars=tab:»\ ,trail:·
" }}}2

" Mouse settings {{{2
set mousehide " hide the cursor while typing
set mouse=a " enable mouse inside vim
" }}}2

" Neovim's  Terminal settings {{{2
if has('nvim')
  highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
  highlight! Normal ctermfg=252 ctermbg=233 guifg=#F8F8F2 guibg=#1B1D1E " change background to be darker
endif
" }}}2

" Search Settings {{{2
set hlsearch " highlight all matches
set ignorecase " case of letters are ignore
set incsearch " while typing the pattern, show where it matches
set smartcase " override the ignorecase when search pattern has upper case
set scrolloff=5 " number of screen lines to keep above and below the cursor
" }}}2

" Split Settings {{{2
set splitbelow
set splitright
" }}}2

" Sound Settings {{{2
set noerrorbells
set visualbell
set vb t_vb= " this disable the sound and flash from noerrorbells and visualbell
" }}}2

" Wildmenu Settings {{{2
set wildmenu
set wildmode=list:longest,list:full
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
" }}}2

" Window Settings {{{2
set title " Add the window title with the name of the file that is editing
set guioptions-=T " disable toolbar
set guioptions-=r " disable right-hand scrollbar
set guioptions-=L " disable left-hand scrollbar
set laststatus=2 " Always show the status line
set lazyredraw " don't update the display while executing macros
set number " show the current line number
set relativenumber " show the line number relative to the line with the cursor

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor " Integrate with ag
endif
" }}}2

" Wrap Settings {{{2
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=80
" }}}2
" }}}1

" Mappings {{{1

" Remap leader to <space> {{{2
let mapleader = " "
let maplocalleader = ","
" }}}2

" Double spaces remove highlight {{{2
nnoremap <silent> <leader><space> :noh<cr>
" }}}2

" Disable Arrow Keys {{{2
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
" }}}2

" Mappings for F1-12 {{{2

" disable F1 to not call help
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

nnoremap <F6> :CocCommand explorer<CR>
nnoremap <F8> :TagbarOpenAutoClose<CR>
" }}}2

" Editing a file {{{2

" Remap Line motion commands
nnoremap k gk
nnoremap gk k
nnoremap j gj
nnoremap gj j

" change <c-i> mapping because of coc
nnoremap <C-o> <Nop>
nnoremap <A-i> <C-i>
nnoremap <A-o> <C-o>

" Save current file
" nnoremap <leader>s :w<cr>
nnoremap <leader>s :call SaveCurrentBuffer()<cr>
nnoremap <c-s> :call SaveCurrentBuffer()<cr>

" Save all files
nnoremap <leader>S :wall<cr>

" Save files with sudo
cnoremap w!! w !sudo tee % >/dev/null

" Add ';' at the end of line
inoremap <leader>; <C-o>m`<C-o>A;<C-o>``

" Toggle Paste
nnoremap <leader>tp :set invpaste paste?<cr>
set pastetoggle=<leader>tp

" Toggle Wrap
nnoremap <leader>tw :set wrap! wrap?<cr>

" Find duplicate words
nnoremap <leader>fW /\v<(\w+)\_s+\1><cr>

" Find current word
nnoremap <leader>fw /\v<C-r><C-w><cr>

" Find and Substitute current word
nnoremap <leader>fs :%s/\v<C-r><C-w>

" Close tags
inoremap <// </<C-X><C-O>

" insert html tags
inoremap <c-e> <></><Esc>5hdiwp3lpT>i
nnoremap <c-e> a<></><Esc>5hdiwp3lpT>i
" }}}2

" Buffer Mappings {{{2

" Creating a new buffer
nnoremap <leader>bn :vnew<cr>  |" Open a new buffer in a new vertical window
nnoremap <leader>bN :new<cr>   |" Open a new buffer in a new horizontal window

" Deleting the buffer
nnoremap <leader>bd :bd<cr>   |" Delete the buffer
nnoremap <leader>bD :bd!<cr>  |" Forces to delete the buffer

" Moving around the buffers
nnoremap [b :bp<cr>              |" go to the previous buffer
nnoremap ]b :bn<cr>              |" go to the next buffer
nnoremap [B :bfirst<cr>          |" go to the first buffer
nnoremap ]B :blast<cr>           |" go to the last buffer
nnoremap <leader>bh :bp<cr>      |" Move to the previous buffer
nnoremap <leader>bl :bn<cr>      |" Move to the next buffer
nnoremap <leader>bH :bfirst<cr>  |" Move to the previous buffer
nnoremap <leader>bL :blast<cr>   |" Move to the next buffer
nnoremap <leader>ba <c-^><cr>    |" Alternate between buffers

" }}}2


" Windows Mappings {{{2

" Opening and closing a window
nnoremap <leader>wc <c-w>c     |" close the window
nnoremap <leader>wn :vnew<cr>  |" new horizontally window
nnoremap <leader>wN :new<cr>   |" new vertically window

" splitting current window
nnoremap <leader>ws <c-w><c-s><cr>  |" split curent window horizontally
nnoremap <leader>wv <c-w><c-v><cr>  |" split current window vertically

" change cursor around windows
nnoremap [w <c-w>W              |" go to the previous window
nnoremap ]w <c-w><c-w>          |" go to the next window
nnoremap <leader>wa <c-w><c-p>  |" Alternate between windows
nnoremap <leader>wo <c-w>o      |" window only
nnoremap <leader>wr <c-w>r      |" window rotate
nnoremap <leader>wx <c-w>x      |" window eXchange

" move windows around the screen
nnoremap <leader>wmj <c-w>J  |" move to bottom window
nnoremap <leader>wmk <c-w>K  |" move to top window
nnoremap <leader>wmh <c-w>H  |" move to left window
nnoremap <leader>wml <c-w>L  |" move to right window
nnoremap <leader>wmt <c-w>T  |" move current window to a tab

" resize windows
nnoremap <leader>wW <c-w>\|          |" Expand the width of the Window
nnoremap <leader>wH :resize 100<cr>  |" Expand the height of the Window
nnoremap <leader>w= <c-w>=           |" Make all windows equally high and wide
" nnoremap <Up>    <c-w>-              |" Decrease current window height
" nnoremap <Down>  <c-w>+              |" Increase current window height
" nnoremap <Left>  <c-w>>              |" Increase current window width
" nnoremap <Right> <c-w><              |" Decrease current window width

" windows switching
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
if has('nvim')
  tnoremap <C-h> <c-\><c-n><c-w>h
  tnoremap <C-j> <c-\><c-n><c-w>j
  tnoremap <C-k> <c-\><c-n><c-w>k
  tnoremap <C-l> <c-\><c-n><c-w>l
endif
" }}}2

" Tabs Mappings {{{2

" Opening and closing a tab
nnoremap <leader>tc :tabclose<cr>  |" close current tab
nnoremap <leader>tn :tabnew<cr>    |" create a new empty tab
nnoremap <leader>to :tabonly<cr>   |" close all others tab but this

" Moving betwen tabs
nnoremap [t :tabp<cr>              |" go to the previous tab
nnoremap ]t :tabn<cr>              |" go to the next tab
nnoremap [T :tabfirst<cr>          |" go to the first tab
nnoremap ]T :tablast<cr>           |" go to the last tab
nnoremap <leader>th :tabp<cr>      |" go to the previous tab
nnoremap <leader>tl :tabn<cr>      |" go to the next tab
nnoremap <leader>tH :tabfirst<cr>  |" go to the first tab
nnoremap <leader>tL :tablast<cr>   |" go to the last tab
nnoremap <A-Left> :tabp<cr>        |" go to the previous tab
nnoremap <A-Right> :tabn<cr>       |" go to the next tab
nnoremap <S-Left> :tabfirst<cr>    |" go to the first tab
nnoremap <S-Right> :tablast<cr>    |" go to the last tab

" Moving the tabs
nnoremap <leader>tmh :tabmove -1<cr>  |" move tab to the left
nnoremap <leader>tml :tabmove +1<cr>  |" move tab to the right
nnoremap <C-S-Left>tmh :tabmove -1<cr>  |" move tab to the left
nnoremap <C-S-Right>tml :tabmove +1<cr>  |" move tab to the right
" }}}2
"
" Neovim's Terminal Mappings {{{2
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
endif
" }}}2

" Vim Mappings {{{2
nnoremap <leader>ve :tabe $MYVIMRC<cr>  |" Edit vimrc file in a new tab
nnoremap <leader>vs :so $MYVIMRC<cr>    |" Source vimrc
" }}}2

" Python Mappings {{{2
if isdirectory('/opt/anaconda3')
  let g:python3_host_prog = '/opt/anaconda3/bin/python'
else
  let g:python3_host_prog = '/usr/local/anaconda3/bin/python'
endif
" }}}2

" clojure-lsp {{{2
function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
endfunction

nnoremap <silent> crcc :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'cycle-coll', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crth :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crtt :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crtf :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crtl :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> cruw :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-thread', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crua :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crml :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'move-to-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
nnoremap <silent> cril :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'introduce-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
nnoremap <silent> crel :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'expand-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> cram :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'add-missing-libspec', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crcn :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'clean-ns', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> crcp :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'cycle-privacy', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> cris :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'inline-symbol', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> cref :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'extract-function', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Function name: ')]})<CR>

" }}}2


" }}}1

" Plugins Settings {{{1

" Plugin airline {{{2
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#buffer_nr_show = 1
" let g:airline#extensions#tabline#formatter = 'unique_tail'
" }}} 2

" Plugin coc.nvim {{{2

let g:coc_global_extensions = [
  \ 'coc-actions',
  \ 'coc-css',
  \ 'coc-cssmodules',
  \ 'coc-conjure',
  \ 'coc-html',
  \ 'coc-html-css-support',
  \ 'coc-emmet',
  \ 'coc-eslint',
  \ 'coc-explorer',
  \ 'coc-git',
  \ 'coc-go',
  \ 'coc-highlight',
  \ 'coc-jest',
  \ 'coc-json',
  \ 'coc-pairs',
  \ 'coc-prettier',
  \ 'coc-python',
  \ 'coc-rust-analyzer',
  \ 'coc-snippets',
  \ 'coc-todolist',
  \ 'coc-tsserver',
  \ 'coc-vimlsp',
  \ 'coc-yaml',
  \ 'coc-yank',
  \ ]

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)


" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
vmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call CocAction('runCommand', 'editor.action.organizeImport')

" Mappings using CoCList:

" Using CocList
nnoremap <silent> <leader>ll :<C-u>CocList<CR>
" Show all diagnostics.
nnoremap <silent> <leader>la  :<C-u>CocList diagnostics<cr>
" Show all buffers
nnoremap <silent> <leader>lb  :<C-u>CocList buffers<cr>
" Show commands.
nnoremap <silent> <leader>lc  :<C-u>CocList commands<cr>
" Manage extensionsleader
nnoremap <silent> <leader>le  :<C-u>CocList extensions<cr>
" Find symbol of culeader cdocument.
nnoremap <silent> <leader>lo  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <leader>ls  :<C-u>CocList -I symbols<cr>
" Do default actionleaderncext item.
nnoremap <silent> <leader>cn  :<C-u>CocNext<CR>
" Do default actionleaderpcrevious item.
nnoremap <silent> <leader>cp  :<C-u>CocPrev<CR>
" Resume latest cocleader.c
nnoremap <silent> <leader>lr  :<C-u>CocListResume<CR>
" call Fold
nnoremap <silent> <leader>cF  :Fold<CR>zR

" Open yank list
nnoremap <silent> <leader>ly  :<C-u>CocList -A --normal yank<cr>
" coc-todo list
nnoremap <silent> <leader>lt  :<C-u>CocList todolist<cr>
nnoremap <silent> <leader>ct  :<C-u>CocList --input=todolist. --normal commands<cr>

" }}} 2

" Plugin conjure {{{2
let g:conjure#mapping#doc_word = v:false
" }}}2

" Plugin python {{{2
if has('nvim')

  if isdirectory('/opt/anaconda3')
    let g:python3_host_prog = '/opt/anaconda3/bin/python'
  else
    let g:python3_host_prog = '/usr/local/anaconda3/bin/python'
  endif

endif
" }}}2

" Plugin Easy-align {{{2
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" }}}2

" Plugin easy-motion {{{2
let g:EasyMotion_do_mapping = 0 " Disable default mappings

map <leader>/ <Plug>(incsearch-easymotion-/)
map <leader>? <Plug>(incsearch-easymotion-?)
map <leader>; <Plug>(incsearch-easymotion-stay)
" }}}2

" Plugin endwise {{{2
let g:endwise_no_mappings = 1
" }}}2

" Plugin FZF {{{2
map <C-p> :Files<cr>
nmap <C-p> :Files<cr>
nnoremap <leader>Ff :files<cr>
nnoremap <leader>FF :GFiles<cr>
nnoremap <leader>Fb :Buffers<cr>
nnoremap <leader>Fa :Ag<cr>
nnoremap <leader>Fl :Lines<cr>
nnoremap <leader>FL :Bines<cr>
nnoremap <leader>Ft :Tags<cr>
nnoremap <leader>FT :BTags<cr>
nnoremap <leader>Fc :Commits<cr>
nnoremap <leader>FC :BCommits<cr>
nnoremap <leader>Fh :History<cr>
nnoremap <leader>FH :Helptags<cr>
nnoremap <leader>F: :History:<cr>
nnoremap <leader>F/ :History/<cr>
nnoremap <leader>Fm :Marks<cr>
nnoremap <leader>FM :Maps<cr>

" Complete from open tmux panes (from @junegunn)
inoremap <expr> <C-x><C-t> fzf#complete( 'tmuxwords.rb -all-but-current --scroll 499 --min 5')

inoremap <expr> <C-x><C-k> fzf#complete ('cat /usr/share/dict/words')

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})

" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'
" }}}2

" Plugin floaterm {{{2
let g:floaterm_keymap_toggle = '<A-t>'
let g:floaterm_keymap_new    = '<A-n>'
let g:floaterm_keymap_prev   = '<A-h>'
let g:floaterm_keymap_next   = '<A-l>'
let g:floaterm_keymap_kill   = '<A-k>'
let g:floaterm_width=0.8
let g:floaterm_height=0.8

nnoremap <silent> <F4> :FloatermToggle<cr>
nnoremap <silent> <A-s> :FloatermSend<cr>
inoremap <silent> <A-s> :FloatermSend<cr>
vnoremap <silent> <A-s> :'<,'>FloatermSend<cr>
" }}}2

" Plugin indentLine {{{2
let g:indentLine_setConceal = 0
" }}}2

" Plugin line-mover {{{2
let g:line_mover_key_up = '<A-Up>'
let g:line_mover_key_down = '<A-Down>'
" }}}2

" Plugin vCoolor {{{2
nnoremap <leader>p :VCoolor<cr>
" inoremap <leader>p :VCoolor<cr>
" }}}2

" Plugin vim-indent-guides {{{2
let g:indent_guides_auto_co2ors = 0
" }}}2

" Plugin rainbow_parentheses {{{2
let g:rbpt_colorpairs = [
      \ ['201', '#FF00FF'],
      \ ['yellow', 'yellow'],
      \ ['cyan', 'cyan'],
      \ ['red', 'firebrick1'],
      \ ]

au VimEnter * RainbowParenthesesActivate
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
" }}}2

" }}}1

" Autocommands {{{1

" Reset all augroup for vimrc
augroup vimrc
  autocmd!

  " Delete trailing spaces before saving
  autocmd BufWritePre * :%s/\s\+$//ge

  " Auto save and Auto Reload
  autocmd FocusGained, BufEnter * :silent! !
  autocmd FocusLost, WinLeave * :silent! w
  autocmd CursorHold * silent! checktime

  " Number and Relative number
  autocmd FocusGained * :set relativenumber
  autocmd FocusLost * :set number norelativenumber

  autocmd InsertEnter * :set number norelativenumber
  autocmd InsertLeave * :set relativenumber

  " View save and restore is used for saving and restoring folding
  " autocmd BufWrite * mkview
  " autocmd BufRead * silent loadview

  " Disable flash error
  if has("gui_running")
    if has("x11")
      autocmd VimEnter * set vb t_vb= " gvim resets it, so I set it up again
    endif
  endif

augroup end

" Autocommands on Plugins {{{2
augroup vimrc

  " Plugin ansible=vim
  au BufRead,BufNewFile */ansible/*.yml set filetype=yaml.ansible

  " Plugin vim-indent-guides
  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=234 guibg=#1c1c1c
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=235 guibg=#262626


augroup END
" }}}2
" }}}1

" FileType {{{1

augroup vimrc

  " Filetype: EJS {{{2
  autocmd BufNewFile,BufRead *.ejs set filetype=html
  " }}}2

  " Filetype: Help {{{2
  autocmd FileType help wincmd J " Open help in bottom window
  " }}}2

  " Filetype: Java {{{2
  autocmd FileType java setlocal ts=4 sts=4 sw=4 noexpandtab
  " }}}2

  " Filetype: Vim {{{2
  autocmd FileType vim setlocal foldmethod=marker
  " }}}2

  " Filetype: Vue {{{2
  autocmd CursorMoved,CursorMovedI *vue call s:setVueFileType()
  " }}}2

augroup END
" }}}1

" Functions {{{1

" Editor Utilities {{{2
function! g:SaveCurrentBuffer()
  execute "Format"
  w
endfunction

" }}}2

" }}}1
