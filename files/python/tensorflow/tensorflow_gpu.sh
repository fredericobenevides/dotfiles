#!/bin/sh

set -e

DOWNLOADS_FOLDER="$HOME/Downloads"

CUDA_VERSION=9.1

CUDA_FILE="cuda_9.1.128_mac"
CUDA_URL="https://developer.nvidia.com/compute/cuda/9.1/Prod/local_installers/$CUDA_FILE"

CUDA_DRIVER_FILE="cudadriver_387.128_macos.dmg"
CUDA_DRIVER_URL="http://us.download.nvidia.com/Mac/cuda_387/$CUDA_DRIVER_FILE"

TENSORFLOW_VERSION="r1.6"
TENSORFLOW_COMMIT_VERSION="5aee07fd0462d00c52efb5d3c86bfb955a9d976e"
TENSORFLOW_TMP_FOLDER="/tmp/tensorflow_pkg"

COMPUTE_CAPABILITY=5.2 # Geforce GTX 980
CPU_FEATURES='--copt=-msse4.2 --copt=-mpopcnt --copt=-maes' # sysctl -a | grep machdep.cpu.features

XCODE_VERSION=8.2
XCODE_FILE='Xcode_8.2'
XCODE_URL="https://download.developer.apple.com/Developer_Tools/Xcode_8.2/$XCODE_FILE.xip"

function clean() {
  rm -rf $TENSORFLOW_TMP_FOLDER
}

function install_requirements() {
  sudo xcode-select -s /Applications/Xcode.app

  pip install six numpy wheel

  brew tap caskroom/versions
  brew cask install java8

  brew install coreutils bazel
}

function download_xcode() {
  echo "\nDownload Xcode from this url $XCODE_URL."
  echo "After Downloading. Click twice and extract the file"
  echo "Press any key after doing that"
  read value
}

function install_xcode() {
  echo "\nInstalling the $XCODE_FILE to /Applications"

  extracted_file="$DOWNLOADS_FOLDER/Xcode.app"
  if [ -d $extracted_file ]; then
    echo "Moving $extracted_file to $DOWNLOADS_FOLDER/$XCODE_FILE.app"
    mv $extracted_file "$DOWNLOADS_FOLDER/$XCODE_FILE.app"

    echo "Moving $DOWNLOADS_FOLDER/$XCODE_FILE.app to /Applications"
    mv "$DOWNLOADS_FOLDER/$XCODE_FILE.app" /Applications

    echo "xcode-select in /Applications/$XCODE_FILE.app"
  else
    echo "Skipping - File don't exist"
  fi

  sudo xcode-select -s "/Applications/$XCODE_FILE.app"

  xcode_version=`xcodebuild -version | grep $XCODE_VERSION`
  if [[ -z ${xcode_version} ]]; then
    echo "Xcode version must be $XCODE_VERSION."
    exit -1
  fi
}

function download_cuda() {
  echo "\nVerifying if download cuda"
  cuda_destination="$DOWNLOADS_FOLDER/$CUDA_FILE.dmg"

  if [ -f $cuda_destination ]; then
    echo "Skipping - Already exist"
  else
    echo "Downloading $CUDA_FILE"
    curl -fsSL ${CUDA_URL} -o "$DOWNLOADS_FOLDER/$CUDA_FILE.dmg"
  fi
}

function install_cuda() {
  echo "\nVerifying if install cuda"
  cuda_destination=/usr/local/cuda

  if [ -d $cuda_destination ]; then
    echo "Skipping - Already exists"
  else
    echo "Installing cuda"

    cuda_destination="$DOWNLOADS_FOLDER/$CUDA_FILE.dmg"
    hdiutil attach $cuda_destination

    cuda_volume=`hdiutil info | awk '/CUDA/ {print $3}'`
    open "$cuda_volume/CUDAMacOSXInstaller.app"

    echo "\nWaiting the Installing to finish. Press any key to continue."
    read value

    cuda_disk=`hdiutil info | awk '/CUDA/ {print $3}'`
    hdiutil detach $cuda_disk
  fi
}

# function download_cuda_driver() {
#   echo "\nVerifying if download cuda driver"
#   cuda_destination="$HOME/Downloads/$CUDA_DRIVER_FILE"

#   if [ -f $cuda_destination ]; then
#     echo "Skipping - Already exist"
#   else
#     echo "Downloading $CUDA_DRIVER_FILE"
#     curl -fsSL ${CUDA_DRIVER_URL} -o "$HOME/Downloads/$CUDA_DRIVER_FILE.dmg"
#   fi
# }

# function install_cuda_driver() {
#   echo "\nVerifying if install cuda driver"
#   cuda_destination=/usr/local/cuda

#   # if [ -d $cuda_destination ]; then
#   #   echo "Skipping - Already exists"
#   # else
#     echo "Installing cuda driver"

#     cuda_destination="$HOME/Downloads/$CUDA_DRIVER_FILE.dmg"
#     hdiutil attach $cuda_destination

#     cuda_volume=`hdiutil info | awk '/CUDADriver/ {print $3}'`
#     open "$cuda_volume/CUDADriver.pkg"

#     cuda_disk=`hdiutil info | awk '/CUDADriver/ {print $3}'`
#     hdiutil detach $cuda_disk
#   # fi
# }

function download_cudnn() {
  echo "\nDownload the last cuDNN on https://developer.nvidia.com/cudnn"
  echo "Press any key after downloading the file"
  read
}

function install_cudnn() {
  echo "\nVerifying if install cudnn"

  if [ -f /usr/local/cuda/lib/libcudnn_static.a ]; then
    echo "Skipping - Already exists"
  else
    echo "Installing cudnn inside cuda"

    cudnn_file=`find $DOWNLOADS_FOLDER/cudnn*`
    tar -C $DOWNLOADS_FOLDER -xvf $cudnn_file

    sudo cp -RPf $DOWNLOADS_FOLDER/cuda/include/cudnn.h /Developer/NVIDIA/CUDA-$CUDA_VERSION/include
    sudo cp -RPf $DOWNLOADS_FOLDER/cuda/lib/* /Developer/NVIDIA/CUDA-$CUDA_VERSION/lib
    sudo ln -s /Developer/NVIDIA/CUDA-$CUDA_VERSION/lib/libcudnn* /usr/local/cuda/lib
  fi
}

function set_environment() {
  echo "\nVerifying if install environment variables"

  cuda=
  if [ -f $HOME/.zshrc.local ]; then
    cuda=`grep cuda $HOME/.zshrc.local 2>&1 /dev/null`
  fi

  if [[ ! -z $cuda ]]; then
    echo "Skipping - Already exists"
  else
    echo "Installing environment variables"
    new_env=$(cat <<'EOF'
export CUDA_HOME=/usr/local/cuda \n
export DYLD_LIBRARY_PATH=$CUDA_HOME/lib:$CUDA_HOME/extras/CUPIT/lib \n
export LD_LIBRARY_PATH=$DYLD_LIBRARY_PATH \n
export LD_LIBRARY_PATH=/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH \n
export PATH=$CUDA_HOME/bin:$PATH
EOF
)
    echo $new_env >> $HOME/.zshrc.local
  fi

  export CUDA_HOME=/usr/local/cuda
  export DYLD_LIBRARY_PATH=$CUDA_HOME/lib:$CUDA_HOME/extras/CUPIT/lib
  export LD_LIBRARY_PATH=$DYLD_LIBRARY_PATH
  export PATH=$CUDA_HOME/bin:$PATH
}

function test_cuda() {
  sudo make -C /usr/local/cuda/samples/1_Utilities/deviceQuery
  /usr/local/cuda/samples/bin/x86_64/darwin/release/deviceQuery
}

function clone_tensorflow() {
  echo "\nVerifying if clone tensorflow repository"
  tensorflow=$DOWNLOADS_FOLDER/tensorflow

  current_folder=$PWD

  if [ -d $tensorflow ]; then
    echo "Skipping - Already exists"

    echo "Just checking out to $TENSORFLOW_VERSION"

    cd $DOWNLOADS_FOLDER/tensorflow

    git fetch --all --tags --prune

    git stash
    git checkout master
    git branch -D $TENSORFLOW_VERSION 2> /dev/null
    git checkout $TENSORFLOW_VERSION
    git reset --hard $TENSORFLOW_COMMIT_VERSION
  else
    echo "Cloning tensorflow"
    git clone https://github.com/tensorflow/tensorflow $DOWNLOADS_FOLDER/tensorflow

    cd $DOWNLOADS_FOLDER/tensorflow

    echo "Checkout of branch: $TENSORFLOW_VERSION"
    git fetch --all --tags --prune
    git checkout $TENSORFLOW_VERSION
    git reset --hard $TENSORFLOW_COMMIT_VERSION
  fi

  cd $PWD
}

function install_tensorflow() {
  echo "\nApplying patch to fix eigen bug"
  echo "Copying patchs to $DOWNLOADS_FOLDER/tensorflow"
  cp $HOME/.dotfiles/files/python/tensorflow/patch* $DOWNLOADS_FOLDER/tensorflow

  current_folder=$PWD
  cd $DOWNLOADS_FOLDER/tensorflow

  echo "Applyting patch of protobuf_archive"
  patch -p1 < patch1_workspace.bzl
  echo "Applyting patch of eigen_archive"
  patch -p1 < patch2_workspace.bzl

  # echo "Removing the patch files that were copied"
  rm patch1_workspace.bzl
  rm patch2_workspace.bzl

  echo "Installing Tensorflow with GPU"

  echo "Fixing sizeof(T)) issue"
  sed -i -e "s/ __align__(sizeof(T))//g" tensorflow/core/kernels/concat_lib_gpu_impl.cu.cc
  sed -i -e "s/ __align__(sizeof(T))//g" tensorflow/core/kernels/depthwise_conv_op_gpu.cu.cc
  sed -i -e "s/ __align__(sizeof(T))//g" tensorflow/core/kernels/split_lib_gpu.cu.cc

  echo "Fixing -lgomp issue"
  sed -i.bu '/linkopts = [“-lgomp”]/d' third_party/gpus/cuda/BUILD.tpl

  # echo "Fixing eigen bug"
  # sudo ln -sf /usr/local/cuda/include/crt/math_functions.hpp /usr/local/cuda/include/math_functions.hpp 2> /dev/null

  echo "*****************************"
  echo "Configuring tensorflow"
  echo "Answer only Y for CUDA"
  echo "Change CUDA SDK version 9.0 to $CUDA_VERSION"
  echo "Use Compute Capability: $COMPUTE_CAPABILITY"
  echo "Press any key to continue."
  echo "*****************************"
  read
  ./configure

  echo "Compiling the wheel file with Bazel"
  bazel build --config=cuda --config=opt $CPU_FEATURES \
    --action_env PATH --action_env LD_LIBRARY_PATH --action_env DYLD_LIBRARY_PATH \
    //tensorflow/tools/pip_package:build_pip_package

  echo "Create the wheel"
  bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg

  echo "Install the wheel"
  tensorflow_pkg=`find /tmp/tensorflow_pkg/tensorflow*.whl`
  sudo pip install $tensorflow_pkg

  cd $current_folder
}

clean

install_requirements

download_xcode
install_xcode

download_cuda
install_cuda

download_cudnn
install_cudnn

set_environment

test_cuda

clone_tensorflow
install_tensorflow
