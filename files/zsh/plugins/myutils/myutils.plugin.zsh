_ocrsync_pods() {
  compadd $(oc get pods | grep Running | awk '{print $1}' | head -1 )
}

ocrsync() {
  echo "Running the following command"

  if [ "$2" = "-ucd" ]; then
    echo "CompileDaemon -command=\"oc rsync . $1:$3\""
    CompileDaemon -command="oc rsync . $1:$3"
  else
    echo "\"oc rsync . $1:$2\""
    oc rsync . $1:$2
  fi
}

_ocrsync() {
  _arguments -s -C \
    "-ucd[Use CompileDaemon]" \
    "1:pods:_ocrsync_pods" \
}

compdef _ocrsync ocrsync
