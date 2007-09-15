#!/usr/bin/env perl
use strict;
use CGI qw/:standard/;
use CGI::Carp 'fatalsToBrowser';

$CGI::POST_MAX=1024 * 100;  # max 100K posts
$CGI::DISABLE_UPLOADS = 1;  # no uploads
      
param('url') && param('format') or die "Missing url and/or format parameters.\n";

my $options = '-r html --standalone --reference-links';	
my $url = param('url');
my $format = param('format') || 'markdown';
if ($format =~ /^markdown$/) {
  $options .= ' --strict';
}
if ($format =~ /^markdown\+$/) {
  $format = 'markdown';
}

# Validate URL and format
unless ($url =~ /^(https?:\/\/)?[\w#_-]+(\.[\w#_-]+)+[\w\/#=?_.-]*$/) {
  die "Illegal URL: $url\n" ;
}
unless ($format =~ /^markdown\+?|rst|latex|context|rtf|man|docbook$/) {
  die "Illegal format: $format\n";
}

# Note - pass through head to truncate file to 100K if greater.
# This should prevent certain kinds of DoS attacks.
my $output = `wget -O- $url | head -c100000 | tidy -asxhtml -utf8 | pandoc -w $format $options`;
if ($output =~ /^\s*$/) {
  print start_html,
        h1("No output"),
        p("Either $url could not be retrieved, or its HTML was too malformed to parse."),
        end_html;
  exit 0;
}
print header(-charset=>"utf8",-type=>"text/plain"),
      $output;
