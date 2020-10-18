#!/bin/sh

~/github/patchparse/_build/default/cli/main.exe \
 --log_config_file ~/github/patchparse/log_config.json \
 --git . --gitcommitlist\
 --min 3 --minf 2 \
 --noall --noev --notex \
 --print-sp \
 buglist
