let
  pkgs = import <nixpkgs> { };

in
  { katas = pkgs.haskellPackages.callPackage ./katas.nix { };
  }
