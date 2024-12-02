{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs.haskellPackages; [ cabal-fmt ];
  languages.haskell = {
    enable = true;
    stack = null;
  };
}
