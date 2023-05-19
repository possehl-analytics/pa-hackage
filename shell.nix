{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [

    # for our haskell environment
    (pkgs.haskellPackages.ghcWithHoogle (hps: [
      hps.PyF
      hps.error
      hps.validation-selective
      hps.these
      hps.semigroupoids
      hps.profunctors
    ]))
    pkgs.cabal-install
    pkgs.haskell-language-server

    # for the build file
    pkgs.ninja
    pkgs.haskellPackages.mustache
    pkgs.jq
  ];
}