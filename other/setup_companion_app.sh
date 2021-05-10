#!/usr/bin/env bash

curl -Lo /tmp/net.downloadhelper.coapp-1.6.1-1_amd64.tar.gz https://github.com/mi-g/vdhcoapp/releases/download/v1.6.1/net.downloadhelper.coapp-1.6.1-1_amd64.tar.gz

tar xf /tmp/net.downloadhelper.coapp-1.6.1-1_amd64.tar.gz -C /tmp
/tmp/net.downloadhelper.coapp-1.6.1/bin/net.downloadhelper.coapp-linux-64 install --user
