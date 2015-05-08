" Remap leader to <space>
let mapleader = " "

" Double spaces remove highligh
map <silent> <leader><space> :noh<CR>

" Disable arrow key
nnoremap <Up>    :echoe "Use k"<CR>
nnoremap <Down>  :echoe "Use j"<CR>
nnoremap <Left>  :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up>    <NOP>
nnoremap <Down>  <NOP>
nnoremap <Left>  <NOP>
nnoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>
inoremap <Left>  <NOP>
inoremap <Right> <NOP>

" Save files with sudo
cmap w!! w !sudo tee % >/dev/null

" Quit all windows
map Q :qall<cr>

" Treat long lines as break lines
map j gj
map k gk

" Move lines
nnoremap <silent> <C-j> :m .+1<CR>==
nnoremap <silent> <C-k> :m .-2<CR>==
inoremap <silent> <C-j> <Esc>:m .+1<CR>==gi
inoremap <silent> <C-k> <Esc>:m .-2<CR>==gi
vnoremap <silent> <C-j> :m '>+1<CR>gv=gv
vnoremap <silent> <C-k> :m '<-2<CR>gv=gv

" Paste
noremap <F2> :set invpaste paste?<cr>
set pastetoggle=<F2>

" Buffers Management
map <leader>bn :vnew<cr>
map <leader>bd :bd!<cr>
map <leader>bh :bp<cr>
map <leader>bl :bn<cr>
map <leader>bs <c-^><cr>

" Tabs management
map <leader>tc :tabclose<cr>
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>th :tabmove -1<cr>
map <leader>tl :tabmove +1<cr>
noremap <c-h> :tabp<cr>
noremap <c-l> :tabn<cr>

" Windows Management
map [w  <c-w>W
map ]w  <c-w><c-w>
map <leader>wj <c-w>j
map <leader>wk <c-w>k
map <leader>wh <c-w>h
map <leader>wl <c-w>l
map <leader>wo <c-w>o
map <leader>wr <c-w>r
map <leader>wt <c-w>T
map <leader>wc <c-w><c-v>

" Window Resizing
map <leader>- <C-w>3-
map <leader>= <C-w>=
map <leader>+ <C-w>3+

" Vimrc
nmap <leader>vl :tabe $MYVIMRC<CR>
nmap <leader>vs :so $MYVIMRC<CR>
