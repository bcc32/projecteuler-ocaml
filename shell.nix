{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    # For opam
    m4
    opam
    pkgconfig
    # For this package
    gmp
  ];
}
