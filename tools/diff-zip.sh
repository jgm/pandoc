#!/bin/sh

# This script allows you to compare two epub, odt, or docx
# containers, ignoring insignificant formatting differences
# in the XML contents.

UNAME=$(uname)
if [ "$UNAME" = "Darwin" ]; then
    FIND="find -E"
else
    FIND="find -regextype posix-extended"
fi

f1="$1"
f2="$2"
test -f "$f1" -a -f "$f2" || {
    echo "Usage: diff-zip firstfile secondfile" && exit 1
}
WORKDIR=$(mktemp -d -t diff-zip.XXX)
trap "{ rm -r $WORKDIR; }" EXIT
unzip -q -d "$WORKDIR/a" "$f1"
unzip -q -d "$WORKDIR/b" "$f2"
cd "$WORKDIR"
mkdir tidy
for x in a b; do
    cp -r $x tidy/
    $FIND $x -iregex '.*\.(xhtml|xml|rdf|rels)' -exec sh -c 'mkdir -p "$(dirname tidy/$1)" && tidy -q -xml -utf8 -i "$1" > "tidy/$1"' _ {} \;
done
cd tidy
mkdir c
cp -r a/* c/
cp -r b/* c/
find c -type f -exec sh -c 'echo -e "\033[1m=== ${1#*/} ===\033[0m" ; diff -u "a/${1#*/}" "b/${1#*/}" 2>&1' _ {} \;
