{
  description = "Solutions to ProjectEuler problems in OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:nix-ocaml/nix-overlays";
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, flake-utils, nixpkgs, ocaml-overlays }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ocaml-overlays.overlays.default ];
        };
      in with pkgs; rec {
        devShells.default = mkShell {
          inputsFrom = [ packages.default ];
          buildInputs = lib.optional stdenv.isLinux inotify-tools ++ [
            ocamlPackages.merlin
            ocamlformat
            ocamlPackages.ocp-indent
            ocamlPackages.utop
          ];
        };

        packages.default = ocamlPackages.buildDunePackage rec {
          pname = "euler";
          version = "0.5.0";
          useDune2 = true;
          src =
            nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
          postPatch = ''
            patchShebangs sol/module-list.pl
          '';
          buildInputs = with ocamlPackages; [
            bignum
            cmdliner
            core
            core_bench
            delimited_parsing
            expect_test_helpers_core
            perl
            re
          ];
          checkInputs = [ shellcheck ];
          passthru.checkInputs = checkInputs;
          meta = { homepage = "https://github.com/bcc32/projecteuler-ocaml"; };
        };
      });
}
