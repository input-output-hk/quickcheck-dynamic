#!/bin/bash
find quickcheck-dynamic -type f -name *.hs | xargs -n1 fourmolu -i -c -o -XImportQualifiedPost
find quickcheck-dynamic-iosim -type f -name *.hs | xargs -n1 fourmolu -i -c -o -XImportQualifiedPost
