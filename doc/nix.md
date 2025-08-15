---
title: Using NiX to develop pandoc
author: John MacFarlane
---

The source directory contains `shell.nix` and `flake.nix`,
so if you have NiX installed, you can use either `nix shell`
or `nix develop` can be used to obtain a shell with pandoc
dependencies installed.

To set up `direnv` so that the NiX shell is automatically
activated whenever the directory is entered, add the following
`.envrc` in the pandoc source directory:

```
if ! has nix_direnv_version || ! nix_direnv_version 2.2.1; then
    source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.2.1/direnvrc" "sha256-zelF0vLbEl5uaqrfIzbgNzJWGmLzCmYAkInj/LNxvKs="
fi
use flake
```

This uses [nix-direnv] for caching.  See its web page for
further help.

[nix-direnv]: https://github.com/nix-community/nix-direnv

