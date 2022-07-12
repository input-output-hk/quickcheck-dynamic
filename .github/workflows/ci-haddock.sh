#!/usr/bin/env bash
set -e

cabal haddock --haddock-tests all

[ ! -d docs/ ] && mkdir -p docs/

doc_indices=$(find dist-newstyle/build -name html -a -type d)

for index in ${doc_indices}; do
  echo "Copying ${index}/* to docs/"
  cp -fr ${index}/* docs/
done
