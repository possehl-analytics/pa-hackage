{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
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
    pkgs.ninja
  ];
}
