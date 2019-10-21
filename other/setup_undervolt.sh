#!/usr/bin/env bash

pip install undervolt

python_version="python`python --version | grep -Po '\d.\d'`"

chmod +x ~/.local/lib/${python_version}/site-packages/undervolt.py

sudo ln -sf ~/.local/lib/${python_version}/site-packages/undervolt.py /usr/local/sbin/undervolt.py

sudo cp files/undervolt.service /etc/systemd/system/
sudo cp files/undervolt.timer /etc/systemd/system/

sudo systemctl start undervolt
sudo systemctl start undervolt.timer

sudo systemctl enable undervolt.timer
