#!/bin/bash -e

source package-list.sh

for pkg in "${pkgs[@]}"
do
    (
    cd "./$pkg"
    rm -f dist/*.gz
    cabal check
    cabal sdist
    mv dist/*.gz ..
    )
done
