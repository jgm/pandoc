#!/bin/bash

set -eu

(for i in "$@"; do
  dotnet run --configuration=Release --framework=net8.0 --no-build --no-restore --project OOXML-Validator/OOXMLValidatorCLI -- "${i}" -r
done) >validation
jq <validation

[ $(cat validation | wc -c) = 2 ]
