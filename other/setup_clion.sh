#!/usr/bin/env bash

set -e

CLION_EXTRACTED="clion-2020.3.3"

# curl -Lo /tmp/CLion.tar.gz https://download.jetbrains.com/cpp/CLion-2020.3.3.tar.gz

tar -xzvf /tmp/CLion.tar.gz -C /tmp

sudo mv /tmp/$CLION_EXTRACTED /opt/clion

sudo ln -s /opt/clion/bin/clion.sh /usr/local/bin/clion.

sudo rm -rf /tmp/CLion.tar.gz
