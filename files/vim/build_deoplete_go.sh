#! /bin/bash

. ~/.zshrc

is_linux=`uname -a | grep Linux`;

if [[ -z $is_linux ]]; then
  exit 0
fi

conda_vim=`conda env list | grep vim`;

if [[ -z $conda_vim ]]; then
  echo "Creating conda environment for vim";
  conda create -n vim python=3 gcc_linux-64
fi

echo "Activating conda's vim environment";
conda activate vim

echo "Building deoplete-go";
cd ~/.local/share/nvim/plugged/deoplete-go
make

echo "Deactivating conda's vim environment";
conda deactivate

exit 0
