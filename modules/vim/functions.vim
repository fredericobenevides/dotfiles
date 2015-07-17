"-------------------------------------------------------------------------------
"  NERDTree
"-------------------------------------------------------------------------------

" Open NERDTree and put the focus on the next window
function! g:OpenNerdTree()

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
