#!/bin/bash

ansible-playbook --ask-become-pass -i ansible/hosts ansible/setup.yaml --extra-vars "dotfilespath=`pwd`"
