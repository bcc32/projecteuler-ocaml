{ lib, nix-gitignore, buildDunePackage, bignum, cmdliner, core, core_bench
, delimited_parsing, expect_test_helpers_core, perl, re, shellcheck }:

buildDunePackage rec {
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
  passthru.checkInputs = checkInputs;
  meta = { homepage = "https://github.com/bcc32/projecteuler-ocaml"; };
}
