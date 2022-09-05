#!/bin/bash
find . -type f -name *.hs | xargs -n1 fourmolu -i -c
