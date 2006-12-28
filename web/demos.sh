#!/bin/sh -e
# creates demo page for pandoc
# argument is the destination directory

DEST=$1

NEWLINE='
'
DEMOS='pandoc README -o demo0.html
pandoc -s -S README -o demo0.html
pandoc -s -S -c main.css -B header.html -A footer.html README -o demo0.html
pandoc -s README -o demo0.tex
pandoc -s README.tex -o demo0.txt
pandoc -s -w rst README -o demo0.txt
pandoc -s README -o demo0.rtf 
pandoc -s -m -i -w s5 S5DEMO -o demo0.html
web2markdown http://www.gnu.org/software/make/ -o demo0.txt
markdown2pdf README -o demo0.pdf'

oldifs=$IFS
IFS=$NEWLINE
set -- $DEMOS
IFS=$oldifs

cd $DEST
PATH=../..:$PATH

echo '% Pandoc demos

To see the output created by each of the commands below,
click on the name of the output file:

'
num=0
for command in "$@"; do
    num=$((num + 1))
    command=$(echo $command | sed -e "s/0/$num/")
    firstpart=$(echo $command | sed -e 's/\(.*\) [^ ]* -o.*/\1/')
    input=$(echo $command | sed -e 's/.* \([^ ]*\) -o.*/\1/') 
    output=$(echo $command | sed -e 's/.*-o \(.*\)/\1/')
    echo "1. <code>$firstpart <a href=\""$input"\" title=\""View input file"\">$input</a> -o <a href=\""$output"\" title=\""View pandoc output"\">$output</a></code>"
    result=$($command) # run the command and create output file
    echo "Created $output." >&2
done

