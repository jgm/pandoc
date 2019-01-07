#!/bin/sh
# generate preliminary list of changes since changelog
# was last modified

lastmod=`git log -n2 --format=oneline changelog | awk '{print $1;}'`
#git log --format=oneline $starthash..HEAD
files=`git ls-tree -r master --name-only`
for x in $files
do
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
        GIT_PAGER=cat git log --pretty=format:'%n%w(78,4,6)+ %s (%aN)%n%n%w(78,6,6)%b%n' -- "$lastmod..HEAD" "$x"
    fi
done

