#!/bin/bash

VERSION=$1
FULLNAME=pandoc-$VERSION
read -s -p "Token (https://github.com/settings/applications): "  TOKEN

curl -H "Authorization: token $TOKEN" \
     -H "Accept: application/vnd.github.manifold-preview" \
     -H "Content-Type: application/x-apple-diskimage" \
     --data-binary @$FULLNAME.pkg.zip \
     "https://uploads.github.com/repos/jgm/pandoc/releases/$VERSION/assets?name=$FULLNAME.pkg.zip"

curl -H "Authorization: token $TOKEN" \
     -H "Accept: application/vnd.github.manifold-preview" \
     -H "Content-Type: application/x-msi" \
     --data-binary @$FULLNAME.msi \
     "https://uploads.github.com/repos/jgm/pandoc/releases/$VERSION/assets?name=$FULLNAME.msi"

