#!/bin/bash

# HFST:

wget "http://downloads.sourceforge.net/project/hfst/hfst/archive/hfst-3.8.1.tar.gz"
gunzip hfst-3.8.1.tar.gz
tar xf hfst-3.8.1.tar
cd hfst-3.8.1/
./configure
make
sudo make install
cd ..
echo "HFST install done"


# omorfi:
git clone https://code.google.com/p/omorfi/
cd omorfi
git checkout stable
./autogen.sh
./configure
make
sudo make install
cd ..
echo "Omorfi install done"
