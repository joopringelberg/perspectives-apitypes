#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago


##### PERSPECTIVES-UTILITIES
cd perspectives-utilities

rm -Rf v UTILITIES
ln -s ../../../perspectives-utilities UTILITIES

cd ..

##### PERSPECTIVES-UTILITIES
cd serializablenonemptyarray

rm -Rf v SERIALIZABLENONEMPTYARRAY
ln -s ../../../serialisable-nonempty-arrays SERIALIZABLENONEMPTYARRAY

cd ..

cd ..

