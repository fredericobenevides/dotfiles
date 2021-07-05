#!/usr/bin/env bash

set -e

CLION_VERSION="2021.1.2"
CLION_EXTRACTED="clion-$CLION_VERSION"

curl -Lo /tmp/CLion.tar.gz https://download.jetbrains.com/cpp/CLion-$CLION_VERSION.tar.gz

tar -xzvf /tmp/CLion.tar.gz -C /tmp

sudo mv /tmp/$CLION_EXTRACTED /opt/clion

sudo ln -s /opt/clion/bin/clion.sh /usr/local/bin/clion.

sudo rm -rf /tmp/CLion.tar.gz
