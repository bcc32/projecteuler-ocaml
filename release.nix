{ pkgs ? import <nixpkgs> { } }:

pkgs.ocamlPackage.callPackage ./. { }
