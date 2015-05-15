#!/bin/bash
export PATH=$PATH:/usr/local/bin

# abort if we're already inside a TMUX session
[[ "$TMUX" == "" ]] || exit 0

# startup a "default" session if none currently exists
tmux has-session -t _default || tmux new-session -s _default -d

# present menu for user to choose which workspace to open
PS3="Please choose your session: "
options=($(tmux list-sessions -F "#S") "New session" "zsh")

echo "Available sessions"
echo "------------------"
echo " "
select opt in "${options[@]}"
do
  case $opt in
    "New session")
      read -p "Enter new session name: " session_name
      tmux new -s "$session_name"
      break
      ;;
    "zsh")
      zsh
      break;;
    *)
      tmux attach-session -t $opt
      break
      ;;
  esac
done
