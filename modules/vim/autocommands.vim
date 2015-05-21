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

autocmd FileType java setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType java highlight SpecialKey ctermbg=bg guibg=bg


"-------------------------------------------------------------------------------
"  Disable flash error
"-------------------------------------------------------------------------------
autocmd VimEnter * set vb t_vb= " gvim resets it, so I set it up again

"-------------------------------------------------------------------------------
"  NERDTree
"-------------------------------------------------------------------------------
autocmd VimEnter * call s:OpenNerdTree()





" Open NERDTree and put the focus on the next window
function s:OpenNerdTree()

  " if is a directory, open the NERDTree
  let path = expand("%")
  if path =~ "NERD_tree_1"
    NERDTree
  end

  " NERDtree is open? move to next window, deletes the buffer and goes back
  let path = expand("%")
  if path =~ "NERD_tree_2"
    wincmd p
    bd
    wincmd p
  else
    wincmd p
  end
endfunction
