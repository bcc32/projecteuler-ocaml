use 5.016;
use warnings;

my @modules;

opendir(my $dh, '.');
while (readdir($dh)) {
  if (/\A (?<module_name> sol [^.]*) [.]ml \z/xms) {
    push @modules, ucfirst $+{module_name};
  }
}

say "  (module $_);" for sort @modules;
