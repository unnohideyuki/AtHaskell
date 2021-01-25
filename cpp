#!/bin/bash -v
f=$1
bname=`basename $f .cpp`
g++ -O2 -Wall -o $bname $f