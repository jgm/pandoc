#!/bin/sh
# Modified by edwintorok from https://github.com/devoidfury/docx-validator
# to look at more files than just document.xml.
# Further modified by jgm for portability.
tmpdir=$(mktemp -d)
error_files=""
errors=0
for file in "$@"; do
  file_errors=0
  echo "*** Checking $file"
  rm -rf "$tmpdir"
  unzip -q -o -j "$file" "word/*.xml" -d "$tmpdir"
  for i in "$tmpdir"/*.xml ; do
    xmllint --format "${i}" > "${i}.pretty.xml"
  done
  XSD="./docx-validator/schemas/microsoft/wml-2010.xsd"
  for i in "$tmpdir"/*.pretty.xml; do
    xmllint -noout -nonet \
      -schema "${XSD}" \
      "${i}" 2>&1 || file_errors=$((file_errors + 1))
  done
  if [ $file_errors -gt 0 ]; then
      errors=$((file_errors + errors))
      error_files="$error_files\n$file"
  fi
done
if [ $errors -gt 0 ]; then
   echo "These files failed validation:$error_files"
fi
exit $errors
