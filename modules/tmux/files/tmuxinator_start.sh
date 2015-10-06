#!/bin/bash
export PATH=$PATH:/usr/local/bin

clear

# present menu for user to choose which workspace to open
PS3="Please choose your tmuxinator: "
options=("zsh" "New tmuxinator" $(tmuxinator list | sed '1d') )

echo "Available tmuxinator"
echo "------------------"
echo " "
select opt in "${options[@]}"
do
  case $opt in
    "New tmuxinator")
      read -p "Enter new tmuxinator name: " tmuxinator_name
      tmuxinator open "$tmuxinator_name"
      break
      ;;
    "zsh")
      zsh
      break
      ;;
    *)
      echo "------------------"
      echo "Run or Edit?"
      echo "------------------"
      echo " "

      subOptions=("run" "edit")
      select  subOptions in "${subOptions[@]}"
      do
        case $subOptions in
          "run")
            tmuxinator $opt
            break
            ;;
          "edit")
            tmuxinator open $opt
            break
            ;;
        esac
      done
      break
      ;;
  esac
done
