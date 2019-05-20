ocrsync() {
  echo "Running the following command"

  if [ "$1" = "-sd" ]; then
    command="reflex -s -r '\.go$' -- sh -c \"go build -o main && oc rsync . --no-perms=true $2:$3\" $4"
    echo $command
    eval $command
  elif [ "$1" = "-sf" ]; then
    command="reflex -s -r '\.go$' -- sh -c \"go build -o main && oc rsync . --no-perms=true --exclude=* --include=$4 $2:$3\" $5"
    echo $command
    eval $command
  else
    echo "\"oc rsync . --no-perms=true $1:$2\""
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
        "-sf:Use reflex to sync file"
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
