#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git --quiet https://github.com/input-output-hk/haskell.nix > nix/haskell.nix-src.json
