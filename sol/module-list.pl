#!/usr/bin/env perl
use 5.016;
use warnings;

my @modules;

for (@ARGV) {
  if (/\A (?<module_name> sol [^.]*) [.]ml \z/xms) {
    push @modules, ucfirst $+{module_name};
  }
}

print <<HEADER;
open! Core
open! Import

let all : (module Solution.S) list = [
HEADER

say "  (module $_);" for sort @modules;

print <<FOOTER;
]
;;
FOOTER
