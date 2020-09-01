#!/bin/bash

export WNHOME=/home/jvs/open/shlurd/src/test/resources/nanonet
rm ${WNHOME}/*
/home/jvs/open/extjwnl-2.0.2/bin/ewn -script create.txt
