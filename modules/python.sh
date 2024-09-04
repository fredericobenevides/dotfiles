#!/usr/bin/env bash

function install_python() {
    info "Installing and configuring python"

    if [ -d ~/.venv ]; then
        info "python already configured!"
        return
    fi
    
    echo -e "-- Creating python environment"
    python -m venv ~/.venv

    info "Finished installing and configuring python"    
}

function install_pkgs_python() {
    info "Installing python packages"

    echo -e "-- Installing packages required by emacs"
    
    pip "epc"
    pip "orjson"
    pip "sexpdata"
    pip "six"
    pip "setuptools"
    pip "paramiko"
    pip "rapidfuzz"
    
    echo -e "-- Finished installing packages required by emacs"

    info "Finished installing python packages"
}

install_python
install_pkgs_python
