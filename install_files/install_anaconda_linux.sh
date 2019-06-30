#! /bin/bash -x

filename="Anaconda3-2019.03-Linux-x86_64.sh"

install_anaconda() {
  anaconda=`python -V 2>&1 | grep Anaconda`
  if [[  -d "/opt/anaconda3" ]]; then
    echo "Already installed"
  else
    curl -L https://repo.anaconda.com/archive/${filename} -o /tmp/$filename
    chmod +x /tmp/$filename
    sudo /tmp/$filename -b -p /opt/anaconda3
    sudo chown -R `whoami`:`whoami` /opt/anaconda3

    rm /tmp/$filename

  fi

  exit 0
}

install_anaconda
