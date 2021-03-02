{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let pkg = ocamlPackages.callPackage ./. { };
in mkShell {
  inputsFrom = [ pkg ];
  buildInputs = pkg.checkInputs ++ [
    ocamlPackages.merlin
    ocamlformat
    ocamlPackages.ocp-indent
    ocamlPackages.utop
  ];
}
