#!/bin/sh -e
# creates example page for pandoc
# argument #1 is the destination directory
# argument #2 is the directory containing pandoc, html2markdown, markdown2pdf

DEST=$1
PROGPATH=$2

NEWLINE='
'
EXAMPLES='HTML fragment:
pandoc README -o example0.html
Standalone HTML file:
pandoc -s -S README -o example0.html
HTML with smart quotes, CSS, and custom header and footer:
pandoc -s -S -c main.css -B header.html -A footer.html README -o example0.html
LaTeX:
pandoc -s README -o example0.tex
From LaTeX to markdown:
pandoc -s README.tex -o example0.txt
reStructuredText:
pandoc -s -w rst README -o example0.txt
Rich text format (RTF):
pandoc -s README -o example0.rtf 
S5 HTML slide show (all in one file):
pandoc -s -m -i -w s5 S5DEMO -o example0.html
Docbook XML:
pandoc -s -w docbook README -o example0.db
Converting a web page to markdown:
html2markdown http://www.gnu.org/software/make/ -o example0.txt
From markdown to PDF:
markdown2pdf README -o example0.pdf
PDF with numbered sections and a custom LaTeX header:
markdown2pdf -N -C myheader.tex README -o example0.pdf'

oldifs=$IFS
IFS=$NEWLINE
set -- $EXAMPLES
IFS=$oldifs

cd $DEST

echo '% Pandoc examples

To see the output created by each of the commands below,
click on the name of the output file:

'
num=0
while [ $# -gt 0 ]; do
    description="$1"
    command="$2"
    num=$(($num + 1))
    command=$(echo $command | sed -e "s/0/$num/")
    firstpart=$(echo $command | sed -e 's/\(.*\) [^ ]* -o.*/\1/')
    input=$(echo $command | sed -e 's/.* \([^ ]*\) -o.*/\1/') 
    output=$(echo $command | sed -e 's/.*-o \(.*\)/\1/')
    echo "1. $description"
    echo
    echo "    <code>$firstpart <a href=\""$input"\" title=\""View input file"\">$input</a> -o <a href=\""$output"\" title=\""View pandoc output"\">$output</a></code>"
    echo $command >&2
    result=$(PATH=$PROGPATH:$PATH $command) # run the command and create output file
    shift
    shift
done

