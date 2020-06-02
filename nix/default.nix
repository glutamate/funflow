let
  haskellNix = import ./haskell.nix-src.nix {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; };
    modules = [
        # The SQLite tests break due to not finding 'echo'. We disable them for now.
        {packages.funflow.components.tests.unit-tests.testFlags = ["-p '! /SQLite/'"];}
    ];
  }
