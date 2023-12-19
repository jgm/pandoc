#!/bin/bash

set -eu

(for i in "$@"; do
  dotnet run --configuration=Release --no-build --no-restore --project OOXML-Validator/OOXMLValidatorCLI -- "${i}" -r
done) >validation
json_reformat -s <validation

[ $(cat validation | wc -c) = 2 ]
