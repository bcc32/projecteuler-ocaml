with import <nixpkgs> { };

let
  inherit (ocamlPackages)
    buildDunePackage bignum cmdliner core core_bench delimited_parsing
    expect_test_helpers_core re;

in buildDunePackage rec {
  pname = "euler";
  version = "0.5.0";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  postPatch = ''
    patchShebangs sol/module-list.pl
  '';
  buildInputs = [
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
  meta = { homepage = "https://github.com/bcc32/projecteuler-ocaml"; };
}
