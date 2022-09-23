#!/usr/bin/env bash

# Modify the version numbers of dependencies as needed. Then run ./bumpVersions.sh to create updated versions of
# * packages.dhall
# * createPerspectivesLinks.sh
# * package.json

UTILITIES=v1.1.0
SERIALIZABLENONEMPTYARRAY=v1.1.0

sed "s/UTILITIES/${UTILITIES}/g;\
s/SERIALIZABLENONEMPTYARRAY/${SERIALIZABLENONEMPTYARRAY}/g;" packages.template.dhall > packages.dhall

sed "s/UTILITIES/${UTILITIES}/g;\
s/SERIALIZABLENONEMPTYARRAY/${SERIALIZABLENONEMPTYARRAY}/g;" createPerspectivesLinks.template.sh > createPerspectivesLinks.sh

