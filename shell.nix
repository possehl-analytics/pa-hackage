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
      hps.scientific
      hps.semigroupoids
      hps.aeson
      hps.aeson-better-errors
      hps.attoparsec
      hps.case-insensitive
      hps.text
      hps.hspec
      hps.nicify-lib
      hps.aeson-pretty
      hps.hscolour
      hps.typed-process
      hps.monad-logger
    ]))
    pkgs.cabal-install
    pkgs.haskell-language-server

    # for the build file
    pkgs.ninja
    pkgs.haskellPackages.mustache
    pkgs.jq
  ];
}
