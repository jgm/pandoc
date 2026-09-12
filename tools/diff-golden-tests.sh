#!/bin/sh -e
for f in `git status test/*/golden | grep modified: | sed -e 's/^.*: *//'`; do git cat-file -p HEAD:$f > $(basename $f) ; ./tools/diff-zip.sh $(basename $f) $f;  done
