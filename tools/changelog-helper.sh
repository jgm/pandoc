#!/bin/sh
# generate preliminary list of changes since changelog
# was last modified

lastmod=`git log -n1 --format=oneline changelog.md | awk '{print $1;}'`
files=`git ls-tree -r master --name-only | grep --invert-match '^test\/\|^data\/docx\/\|^data\/odt\/\|^data\/pptx\/\|^citeproc\/\|^data\/translations\/'`
for x in $files
do
    echo $x 1>&2
    commits=`git log -n1 $lastmod..HEAD $x`
    if [ ! -z "$commits" ]
    then
        if echo $x | grep -q "src\/.*\.hs"
        then
            file=`echo $x | sed -e 's/src\///' | sed -e 's/\//\./g' | sed -e 's/\.hs$//'`
        else
            file=$x
        fi
        echo "  * $file"
        git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" "$x"
    fi
done
echo "test/" 1>&2
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" test/
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" data/docx/
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" data/odt/
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" data/pptx/
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" data/translations/
git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' "$lastmod..HEAD" citeproc/

