use 5.016;
use warnings;

my @modules;

opendir(my $dh, '.');
while (readdir($dh)) {
  if (/\A (?<module_name> sol [^.]*) [.]ml \z/xms) {
    push @modules, ucfirst $+{module_name};
  }
}

print <<HEADER;
open! Core
open! Import

let modules : (module Euler.Solution_intf.S) list = [
HEADER

say "  (module $_);" for sort @modules;

print <<FOOTER;
]
;;
FOOTER
