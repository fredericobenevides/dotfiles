#!/usr/bin/env sh
clear

echo "##################################################"
echo "Configuring the environment"
echo "##################################################"

for i in `cat modules`; do
	cd $i
	sh configure.sh
	cd ..
done

echo "\n\n---------------------------------------------------------------------"
echo "Dotfiles installed. Open a new tab or execute `which zsh` to finish"
echo "---------------------------------------------------------------------" 

echo "\n\n##################################################"
echo "\tFinished!"
echo "##################################################\n\n"
