#!/bin/bash

set -e

PROJECT_DIR_REL=`dirname "${BASH_SOURCE[0]}"`/..
PROJECT_DIR=`readlink -f ${PROJECT_DIR_REL}`
echo ${PROJECT_DIR}

rm -f ${PROJECT_DIR}/src/test/resources/spanish_net/*
rm -f ${PROJECT_DIR}/ewn.xml
cp ${PROJECT_DIR}/src/test/resources/spanish_net.xml ${PROJECT_DIR}/ewn.xml
pushd ${PROJECT_DIR}
../extjwnl-2.0.2/bin/ewn -script "${PROJECT_DIR}/testnet/spanish_net.txt"
rm -f ${PROJECT_DIR}/ewn.xml
popd
