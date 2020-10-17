#!/bin/sh

~/github/patchparse/_build/default/cli/main.exe \
 --git . --gitcommitlist buglist \
 --min 3 --minf 2 \
 --noall --noev --notex \
 --print-sp

