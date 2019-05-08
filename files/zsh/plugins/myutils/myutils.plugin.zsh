ocrsync() {
  echo "Running the following command"

  if [ "$1" = "-sd" ]; then
    echo "reflex -s -g '*.go' -- sh -c \"go build -o main && oc rsync . $2:$3\" $4"
    reflex -s -g '*.go' -- sh -c "go build -o main && oc rsync . $2:$3" $4
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
        "-sd:Use reflex to sync directory"
        "-none:Sync only one time"
      )

      _describe -t commands 'command' commands
    ;;

    pods)
      compadd $(oc get pods | grep Running | awk '{print $1}')
    ;;
  esac
}

compdef _ocrsync ocrsync
