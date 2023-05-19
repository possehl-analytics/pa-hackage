{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithHoogle (hps: [
    ]))
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.ninja
  ];
}
