set -x EDITOR 'nvim'
set -x VISUAL 'nvim'



### ANACONDA
if test -d /opt/anaconda3
  eval /opt/anaconda3/bin/conda "shell.fish" "hook" $argv | source
end



### ANDROID
if test -d /opt/Android/android-studio
  set -x ANDROID_HOME "/opt/Android/android-studio"
  set -x ANDROID_SDK_ROOT "/opt/Android/Sdk"
  set PATH "$ANDROID_SDK_ROOT/platform-tools:$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/tools/bin:$ANDROID_SDK_ROOT/emulator:$PATH"
end



### DOOM-EMACS
if test -d ~/.emacs.d
  set -x EMACS_HOME "$HOME/.emacs.d"
  set PATH "$EMACS_HOME/bin:$PATH"
end



### FLUTTER
if test -d /opt/flutter
  set -x FLUTTER_HOME "/opt/flutter"
  set -x FLUTTER_PUB_CACHE "/opt/flutter/.pub-cache"
  set -x DART_HOME /opt/flutter/bin/cache/dart-sdk

  set PATH "$DART_HOME/bin:$FLUTTER_PUB_CACHE/bin:$FLUTTER_HOME/bin:$PATH"
end



### FZF
if test -d $HOME/.fzf
  set PATH "$HOME/.fzf/bin" $PATH
  set -x FZF_DEFAULT_COMMAND 'ag --hidden --ignore .git --ignore node_modules -g ""'
end



### NODE
set PATH "$HOME/.npm-global/bin" $PATH

if test -d /opt/node
  set PATH "/opt/node/bin" $PATH
end



### PYTHON
set -x PYTHONSTARTUP "$HOME/.pythonrc"



### RBENV
set -x RBENV_ROOT "$HOME/.rbenv"

if test -d "$HOME/.rbenv"
  set PATH "$RBENV_ROOT/bin" $PATH
  status --is-interactive; and source (rbenv init -|psub)
end



### RUST
set PATH $HOME/.cargo/bin $PATH



### SNAP
if test -d /usr/lib/snapd
  set PATH "/var/lib/snapd/snap/bin" $PATH
end



### SDKMAN
set -x SDKMAN_DIR "$HOME/.sdkman"



### LOCAL SCRIPTS
set PATH "$HOME/.local_scripts" $PATH



### Load custom env
if test -f $HOME/.fish.local
  source .fish.local
end
