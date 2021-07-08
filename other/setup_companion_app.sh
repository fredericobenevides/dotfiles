#!/usr/bin/env bash

VERSION=1.6.3

curl -Lo /tmp/net.downloadhelper.coapp-$VERSION-1_amd64.tar.gz https://github.com/mi-g/vdhcoapp/releases/download/v$VERSION/net.downloadhelper.coapp-$VERSION-1_amd64.tar.gz

tar xf /tmp/net.downloadhelper.coapp-$VERSION-1_amd64.tar.gz -C /tmp
/tmp/net.downloadhelper.coapp-$VERSION/bin/net.downloadhelper.coapp-linux-64 install --user
