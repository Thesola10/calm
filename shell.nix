{ pkgs ? import <nixpkgs> {}}:
#
# Nix Shell configuration for compiling a Haskell project with a set
# of Cabal packages.
#

let
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    parsec
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.mkShell {
  name = "haskell";
  buildInputs = [ ghc ];
}
