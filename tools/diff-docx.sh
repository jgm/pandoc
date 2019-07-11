#!/bin/sh
f1="$1"
f2="$2"
test -f "$f1" -a -f "$f2" || {
    echo "Usage: diff-docx first.docx second.docx" && exit 1
}
WORKDIR=$(mktemp -d -t diff-docx)
trap "{ rm -r $WORKDIR; }" EXIT
unzip -q -d "$WORKDIR/a" "$f1"
unzip -q -d "$WORKDIR/b" "$f2"
cd "$WORKDIR"
mkdir tidy
for x in a b; do
    cp -r $x tidy/
    find $x -iname '*.xml' -exec sh -c 'mkdir -p "$(dirname tidy/$1)" && tidy -q -xml -utf8 -i "$1" > "tidy/$1"' _ {} \;
    find $x -iname '*.rels' -exec sh -c 'mkdir -p "$(dirname tidy/$1)" && tidy -q -xml -utf8 -i "$1" > "tidy/$1"' _ {} \;
done
cd tidy
mkdir c
cp -r a/* c/
cp -r b/* c/
find c -type f -exec sh -c 'echo "\033[1m=== ${1#*/} ===\033[0m" ; diff -u "a/${1#*/}" "b/${1#*/}" 2>&1' _ {} \;
