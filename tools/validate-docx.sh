#!/bin/sh
# Modified by edwintorok from https://github.com/devoidfury/docx-validator
# to look at more files than just document.xml.
# Further modified by jgm for portability.
tmpdir=$(mktemp -d)
errors=0
for file in "$@"; do
  echo "*** Checking $file"
  rm -rf "$tmpdir"
  unzip -q -o -j "$file" -d "$tmpdir"
  for i in comments document fontTable footnotes numbering settings styles theme1 webSettings; do
    xmllint --format "$tmpdir/${i}.xml" > "$tmpdir/${i}-pretty.xml"
  done
  XSD="./docx-validator/schemas/microsoft/wml-2010.xsd"
  for i in "$tmpdir"/*-pretty.xml; do
    xmllint -noout -nonet \
      -schema "${XSD}" \
      "${i}" 2>&1 || errors=$((errors + 1))
  done
done
exit $errors
