#!/bin/sh

# based on validate-docx.sh, and adapted for OOXMLValidatorCLI
# Modified by edwintorok from https://github.com/devoidfury/docx-validator
# to look at more files than just document.xml.
# Further modified by jgm for portability.

tmpdir=$(mktemp -d)
error_files=""
errors=0
VALIDATOR=OOXML-Validator/bin/Release/*/OOXMLValidatorCLI

for file in "$@"; do
  file_errors=0
  echo "*** Checking $file"
  dotnet $(VALIDATOR) "${file}" 2>&1 || file_errors=1
  if [ $file_errors -gt 0 ]; then
      errors=$((file_errors + errors))
      error_files="$error_files\n$file"
  fi
done
if [ $errors -gt 0 ]; then
   echo "These files failed validation:$error_files"
fi
exit $errors
