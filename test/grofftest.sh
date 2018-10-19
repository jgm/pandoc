#!/bin/bash

# iterates over specified directory, containing "\w+\.\d"-like files,
# executes pandoc voer them and prints stderr on nonzero return code

if [ $# -ne 2 ]; then
	echo "Not enough arguments"
	exit 1
fi

PANDOC=$1
DIR=$2

$PANDOC --version > /dev/null || { echo "pandoc executable error" >&2 ; exit 1 ; }

ls $2 | egrep "^.+\.[0-9].?$" | while read f ; do
	FILE="$DIR/$f"
	$PANDOC -f man -t native < $FILE 2>&1 > /dev/null
	if [ $? -ne 0 ]; then
		echo "Failed to convert $FILE"
	fi
done
