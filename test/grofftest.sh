#!/bin/bash

# iterates recursively over specified directory, tries to convert
# man pages and prints to stderr on errors.

# if called with two arguments, the first is the path to pandoc,
# and the second is the directory.  if with one argument, it
# is the directory, and pandoc is used from path.

if [ $# -eq 2 ]; then
        PANDOC=$1
        DIR=$2
elif [ $# -eq 1 ]; then
        PANDOC=pandoc
        DIR=$1
else
	echo "Not enough arguments"
	exit 1
fi

$PANDOC --version > /dev/null || { echo "pandoc executable error" >&2 ; exit 1 ; }

for f in `find "$DIR" -name '*.[0-9]'`; do
    ( iconv -f utf8 -t utf8 $f 2>/dev/null || iconv -f latin1 -t utf8 $f ) | \
        $PANDOC --resource-path "$DIR":"$(dirname $f)" -f man -o /dev/null || echo "Failed to convert $f"
done
