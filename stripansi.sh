#!/bin/sh

# use th is to strip out ANSI codes, if you want to pipe
# outputof 'cabal test' to a file.

if which -s gsed; then
  gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"
else
  gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"
fi
