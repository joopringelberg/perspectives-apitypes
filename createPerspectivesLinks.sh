#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago


##### PERSPECTIVES-v1.1.0
cd perspectives-utilities

rm -Rf v v1.1.0
ln -s ../../../perspectives-utilities v1.1.0

cd ..

##### PERSPECTIVES-v1.1.0
cd serializablenonemptyarray

rm -Rf v v1.1.0
ln -s ../../../serialisable-nonempty-arrays v1.1.0

cd ..

cd ..

