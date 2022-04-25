#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago


##### PERSPECTIVES-UTILITIES
cd perspectives-utilities

rm -Rf v v1.0.0
ln -s ../../../perspectives-utilities v1.0.0

cd ..

##### PERSPECTIVES-UTILITIES
cd serializablenonemptyarray

rm -Rf v v1.0.1
ln -s ../../../serialisable-nonempty-arrays v1.0.1

cd ..

cd ..

