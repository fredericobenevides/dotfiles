# Work around when opening tmux to set the new environment to fix the colors
export TERM="xterm-256color"
if [ "$TMUX" ] && [ $TERM = "xterm-256color" ]; then
  export TERM="screen-256color"
fi
