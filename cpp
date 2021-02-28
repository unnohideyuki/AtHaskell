#!/bin/bash -v
f=$1
bname1=`basename $f .cpp`
bname=`basename $bname1 .cc`
g++ -std=gnu++17 -O2 -Wall -Wconversion -I../ac-library -o $bname $f
