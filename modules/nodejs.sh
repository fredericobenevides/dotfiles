#!/usr/bin/env bash

if [ -d ~/.volta ]; then
    info "nodejs already configured!"
    return
fi

info "Installing and configuring nodejs"

echo -e "-- Install volta to manage nodejs"
curl https://get.volta.sh | bash

echo -e "-- Installing nodejs"
sh -c "(volta install node)"

info "Finished installing and configuring nodejs"