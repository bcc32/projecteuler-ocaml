{ pkgs ? import <nixpkgs> { } }:

pkgs.ocamlPackages.callPackage ./. { }
