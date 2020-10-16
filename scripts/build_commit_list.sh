#!/bin/sh

mkdir -p out
rm -f out/*

# --grep=bug 
git log --all -i --grep=bugfix --grep=critic --grep=secur --grep=cve --pretty='format:%H' > buglist
wc -l buglist

#~/work/EXPERIMENTS/patchparse3/patchparse.opt --git . --min 5 --minf 2 --noall --noev --print-sp --notex --gitcommitlist buglist 
