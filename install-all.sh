#!/bin/bash -e

# allow a CABAL env var to override
CABAL=${CABAL:-cabal}

# install testing dependencies
$CABAL install HUnit QuickCheck hspec

pkgs=( conduit
       attoparsec-conduit
       blaze-builder-conduit
       filesystem-conduit
       zlib-conduit
       network-conduit
     )

# install each sub-respository
for pkg in "${pkgs[@]}"; do
  echo "Installing $pkg..."

  (
    cd "./$pkg"

    if ! $CABAL configure --disable-library-profiling --enable-tests --ghc-options="-Wall -Werror"; then
      $CABAL install --only-dependencies
      $CABAL configure --enable-tests --ghc-options="-Wall -Werror" --disable-library-profiling
    fi

    $CABAL build
    $CABAL test
    $CABAL check
    $CABAL haddock
    ./Setup.lhs install
    cabal-src-install --src-only
  )
done
