#!/bin/sh

# use to strip out ANSI codes, if you want to pipe
# outputof 'cabal test' to a file.

sed -e 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g'
