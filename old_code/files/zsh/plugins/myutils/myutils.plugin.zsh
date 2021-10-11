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
  elif [ "$1" = "-s2igolang" ]; then
    sync_s2i_golang $1 $2 $3
  else
    echo "\"oc rsync . --no-perms=true $1:$2\""
    oc rsync . $1:$2
  fi
}

# Sync the current folder for an golan's app to the container s2i-golang
sync_s2i_golang() {
    # For a directory /user_directory/go/src/github.com/user/app return go/src/github.com/user/app
    go_folder="${PWD/*go\/src/go/src}"

    # For a directory /go/src/github.com/user/app return github.com/user/app
    app_folder=${go_folder:7}

    # define the s2i-folder from the container s2i-golang
    s2i_folder="/opt/app-root/src"

    command="reflex -s -r '\.go$' -- sh -c \"go build -o main && oc rsync . --no-perms=true $2:${s2i_folder}/${app_folder}\" $3"
    echo $command
    eval $command

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
        "-s2igolang:Use reflex to sync to s2i-golang without the need to specify destination directory"
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
