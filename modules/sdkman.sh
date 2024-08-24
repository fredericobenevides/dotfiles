#!/usr/bin/env bash

if [ -d ~/.sdkman ]; then
    info "sdkman already configured!"
    return
fi

java_version=17.0.12-tem

info "Installing and configuring sdkman"

echo -e "-- Installing sdkman"
export SDKMAN_DIR="$HOME/.sdkman" && curl -s "https://get.sdkman.io" | bash

echo -e "-- Installing java"
source ~/.sdkman/bin/sdkman-init.sh; sdk install java $java_version

sdk default java $java_version

info "installing and configuring sdkman"