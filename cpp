#!/bin/bash -v
f=$1
bname=`basename $f .cpp`
g++ -O2 -Wall -Wconversion -I../ac-library -o $bname $f
