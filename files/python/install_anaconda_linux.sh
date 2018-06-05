#! /bin/bash -x

filename="Anaconda3-5.2.0-Linux-x86_64.sh"

install_anaconda() {
  anaconda=`python -V 2>&1 | grep Anaconda`
  if [ -z "$anaconda" ]; then
    curl -L https://repo.anaconda.com/archive/${filename} -o /tmp/$filename
    chmod +x /tmp/$filename
    sudo /tmp/$filename -b -p /opt/anaconda3
    sudo chown -R `whoami`:`whoami` /opt/anaconda3
  fi
}

install_anaconda
