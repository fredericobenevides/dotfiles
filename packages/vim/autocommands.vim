" Delete trailing spaces before saving
autocmd BufWritePre * :%s/\s\+$//ge

" Auto save
autocmd CursorHold,CursorHoldI,FocusLost * :wa

" Open help in vertical window
autocmd FileType help wincmd L

" NERDTree
autocmd VimEnter * call s:OpenNerdTree()






" Open NERDTree and put the focus on the next window
function s:OpenNerdTree()

  " if is a directory, open the NERDTree
  let path = expand("%")
  if path =~ "NERD_tree_1"
    NERDTree
  end

  " NERDtree is open? move to next window and deletes the buffer
  let path = expand("%")
  if path =~ "NERD_tree_2"
    wincmd p
    bd
  else
    wincmd p
  end
endfunction
