{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107" }:
let 
  project = import ./default.nix { inherit nixpkgs compiler; }; 
  hls = nixpkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
in 
  nixpkgs.mkShell {
    nativeBuildInputs = project.env.nativeBuildInputs ++ [ hls ];
  }
