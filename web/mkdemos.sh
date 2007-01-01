#!/bin/sh -e
# creates example page for pandoc
# argument #1 is the destination directory
# argument #2 is the directory containing pandoc, html2markdown, markdown2pdf

DEST=$1
PROGPATH=$2

NEWLINE='
'
EXAMPLES='pandoc README -o example0.html
pandoc -s -S README -o example0.html
pandoc -s -S -c main.css -B header.html -A footer.html README -o example0.html
pandoc -s README -o example0.tex
pandoc -s README.tex -o example0.txt
pandoc -s -w rst README -o example0.txt
pandoc -s README -o example0.rtf 
pandoc -s -m -i -w s5 S5DEMO -o example0.html
pandoc -s -w docbook README -o example0.db
html2markdown http://www.gnu.org/software/make/ -o example0.txt
markdown2pdf README -o example0.pdf
markdown2pdf -C myheader.tex README -o example0.pdf'

oldifs=$IFS
IFS=$NEWLINE
set -- $EXAMPLES
IFS=$oldifs

cd $DEST
PATH=$PROGPATH:$PATH

echo '% Pandoc examples

To see the output created by each of the commands below,
click on the name of the output file:

'
num=0
for command in "$@"; do
    num=$(($num + 1))
    command=$(echo $command | sed -e "s/0/$num/")
    firstpart=$(echo $command | sed -e 's/\(.*\) [^ ]* -o.*/\1/')
    input=$(echo $command | sed -e 's/.* \([^ ]*\) -o.*/\1/') 
    output=$(echo $command | sed -e 's/.*-o \(.*\)/\1/')
    echo "1. <code>$firstpart <a href=\""$input"\" title=\""View input file"\">$input</a> -o <a href=\""$output"\" title=\""View pandoc output"\">$output</a></code>"
    echo $command >&2
    result=$($command) # run the command and create output file
done

