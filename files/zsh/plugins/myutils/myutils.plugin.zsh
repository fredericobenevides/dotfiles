_ocrsync_pods() {
  compadd $(oc get pods | grep Running | awk '{print $1}' | head -1 )
}

ocrsync() {
  if [ "$2" = "-ucd" ]; then
    CompileDaemon -command="oc rsync . $1:"
  else
    oc rsync . $1:
  fi
}

_ocrsync() {
  _arguments -s -C \
    "-ucd[Use CompileDaemon]" \
    "1:pods:_ocrsync_pods" \
}

compdef _ocrsync ocrsync
