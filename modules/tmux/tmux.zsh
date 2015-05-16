# Work around when opening tmux to set the new environment to fix the colors
if [ "$TERM" ] && [ $TERM = "xterm-256color" ]; then
  export TERM="screen-256color"
fi
