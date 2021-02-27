#!/usr/bin/env perl

# Breaks down compilation time and memory usage by module.
# To use this script, do
#
# stack clean
# stack build --ghc-options='-dshow-passes' 2>output.txt
# perl parseTimings.pl output.txt

while (<>) {
  if (/!!! (.*) \[(.*)\]: finished in (.*) milliseconds, allocated (.*) megabytes/) {
      $phase = $1;
      $module = $2;
      $milliseconds = $3;
      $megabytes = $4;
      if (!$timings{$module}) {
        $timings{$module} = {'milliseconds' => 0, 'megabytes' => 0};
      };
      $timings{$module}{'milliseconds'} += $milliseconds;
      $timings{$module}{'megabytes'} += $megabytes;
    }
  }
  printf("%10s %10s %s\n", "Time (ms)", "Alloc (Mb)", "LOC", "Module");
  for (keys %timings) {
    $path = $_;
    $path =~ s/\./\//g;
    $path = "src/$path.hs";
    $loc = `wc -l $path 2>/dev/null`;
    if ($loc) {
      $loc =~ s/^\s*(\d+).*/\1/;
      printf("%10d %10d %6d %s\n", $timings{$_}{'milliseconds'}, $timings{$_}{'megabytes'}, $loc, $_);
    }
}
