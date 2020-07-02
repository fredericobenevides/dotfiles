#!/usr/bin/env bash

# disable anaconda because if is active has a problem with the path of lib
source deactivate

sudo pacman -Syu libcups avahi cups hplip system-config-printer sane sane-frontends xsane xsane-gimp ghostscript cups-pdf

yay -S cnijfilter2

sudo gpasswd -a $(id -un) lp
sudo gpasswd -a $(id -un) scanner

# open the browser to add the printer
chromium http://localhost:631/admin
