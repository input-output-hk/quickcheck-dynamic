#!/bin/bash
MODE=${1:-inplace}
fourmolu -m $MODE -c $(find quickcheck-dynamic* -type f -name *.hs) \
  -o -XImportQualifiedPost \
  -o -XTypeApplications \
  -o -XPatternSynonyms
exit $?
