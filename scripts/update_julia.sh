#! /bin/bash

cd ~/.bin

wget https://julialangnightlies-s3.julialang.org/bin/linux/x64/julia-latest-linux64.tar.gz
tar xvf julia-latest-linux64.tar.gz

rm -r julia_old
mv julia julia_old
mv julia-* julia

rm julia-latest-linux64.tar.gz
