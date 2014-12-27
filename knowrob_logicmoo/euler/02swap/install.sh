#!/bin/bash
SCRIPT_DIR=$( cd "$( dirname "$0" )" && pwd )
mkdir -p /opt/eye/src
cp -a $SCRIPT_DIR/euler.yap /opt/eye/src
mkdir -p /opt/eye/lib
pushd /opt/eye/lib
swipl -q -f ../src/euler.yap -g main -- --image eye.pvm
popd
mkdir -p /opt/eye/bin
cp -a $SCRIPT_DIR/eye.sh /opt/eye/bin
