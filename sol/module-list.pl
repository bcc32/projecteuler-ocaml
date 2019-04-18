#!/usr/bin/env perl
use 5.016;
use warnings;

my @modules;

for (glob 'sol*.ml') {
  if (/\A (?<module_name> sol [^.]*) [.]ml \z/xms) {
    push @modules, ucfirst $+{module_name};
  }
}

print <<HEADER;
open! Core
open! Import

let all : (string, (module Solution.S)) List.Assoc.t = [
HEADER

say "  $_.name, (module $_);" for sort @modules;

print <<FOOTER;
]
;;
FOOTER
