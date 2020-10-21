#!/bin/sh

mkdir -p out
rm -f out/*

# --grep=bug --grep=fix
git log --all -i --grep=bugfix --grep=critic --grep=secur --grep=cve --pretty='format:%H' > buglist
wc -l buglist
