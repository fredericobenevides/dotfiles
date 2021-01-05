#!/bin/bash

set -e

#https://www.youtube.com/watch?v=RZdWVntmvI8&ab_channel=DenshiVideo

#https://archived.forum.manjaro.org/t/guide-install-and-configure-optimus-manager-for-hybrid-gpu-setups-intel-nvidia/92196

# sudo mhwd -a pci nonfree 0300

echo "Edit /etc/sddm.conf and comment DisplayCommand and DisplayStopCommand"
read
read

conda deactivate

yay -S optimus-manager optimus-manager-qt

systemctl status optimus-manager.service
