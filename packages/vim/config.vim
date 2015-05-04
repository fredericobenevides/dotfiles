set cursorline
set hidden
set laststatus=2
set number
set relativenumber
set title

" backspace working like the others apps
set backspace=indent,eol,start

" backup
set nobackup
set noswapfile

" Default enconding
set encoding=utf-8
set fileencoding=utf-8

" ESC
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Gui
set guioptions-=T
set guioptions-=r
set guioptions-=L

" History and Undo
set history=500
set undolevels=500

" Indent & Tab
set autoindent
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

" List
set list
set listchars=tab:»·,trail:·

" Searching
set hlsearch
set ignorecase
set incsearch
set smartcase
set scrolloff=5

" Split
set splitbelow
set splitright

" Sound
set noerrorbells
set visualbell
set vb t_vb=

" Wildmenu
set wildmode=list:longest,list:full
set wildmenu
set wildignore+=/tmp/*,*.swp

" Wrap
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=+1
" execute "set colorcolumn=".join(range(81,999), ',')
