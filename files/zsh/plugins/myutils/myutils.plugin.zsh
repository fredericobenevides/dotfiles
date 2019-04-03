ocrsync() {
  echo "Running the following command"

  if [ "$1" = "-ucd" ]; then
    echo "CompileDaemon -command=\"oc rsync . $2:$3\""
    CompileDaemon -command="oc rsync . $2:$3"
  else
    echo "\"oc rsync . $1:$2\""
    oc rsync . $1:$2
  fi
}

_ocrsync() {
  _arguments \
    '1:sync :->sync' \
    '2:pods :->pods'

  case "${state}" in
    sync)
      local commands; commands=(
        "-ucd:Use CompileDaemon - Keep syncing"
        "-ncd:Don't use CompileDaemon - Sync only one time"
      )

      _describe -t commands 'command' commands
    ;;

    pods)
      compadd $(oc get pods | grep Running | awk '{print $1}')
    ;;
  esac
}

compdef _ocrsync ocrsync
