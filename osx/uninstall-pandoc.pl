#!/usr/bin/perl

# Script to remove all files installed by the OSX pandoc installer
# and unregister the package.  Modified from a script contributed
# by Daniel T. Staal.

use warnings;
use strict;

use File::Spec;

# The main info: this is the list of files to remove and the pkg_id.
my $pkg_id    = 'net.johnmacfarlane.pandoc';

my @pkg_info;

# Find which, if any, volume Pandoc is installed on.
my $volume;

# First check /, then other volumes on the box.
my $cur_test = `pkgutil --pkgs=$pkg_id`;
if ( $cur_test =~ m/$pkg_id/ ) {
    $volume = '/';
} else {
    opendir( my $dh, '/Volumes' ) or die "Can't list Volumes: $!\n";
    foreach my $dir ( readdir($dh) ) {
      next if $dir =~ m/^\./;    # Skip dotfiles.

      my $path = File::Spec->rel2abs( $dir, '/Volumes' );
      next if !( -d $path );     # Skip anything that isn't a directory.

      my $cur_test = `pkgutil --pkgs=$pkg_id --volume '$path'`;
      if ( $cur_test =~ m/$pkg_id/ ) {
          $volume = $path;
          last;
      }
    }
}

die "Pandoc not installed.\n" if !( defined($volume) );

my @pkg_files = ();
my $f;
for $f (split '\n', `pkgutil --volume '$volume' --only-files --files $pkg_id`) {
    push @pkg_files, File::Spec->rel2abs($f, $volume);
};

print "The following files will be deleted:\n\n";
print join("\n", @pkg_files);
print "\n\n";
print "Do you want to proceed and uninstall pandoc (Y/N)?";
my $input = <STDIN>;

if ($input =~ m/^[Yy]/) {

    # Actually remove the files.
    foreach $f (@pkg_files) {
        if (system("sudo rm $f") == 0) {
            warn "Deleted $f\n";
        } else {
            warn "Unable to delete $f: $!\n";
            warn "Aborting uninstall.\n";
            exit 1;
        }
    }

    # Clean up the install.
    if (system("sudo pkgutil --forget $pkg_id --volume '$volume'") != 0) {
        warn "Unable to clean up install: $!\n";
        exit 1;
    }

} else {

   print "OK, aborting uninstall.\n";
   exit 0;

}

print "Pandoc has been successfully uninstalled.\n";
exit 0;
