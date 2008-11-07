#!/usr/bin/perl -w
# first argument is input filename - a demo template.
# second argument is output filename.

my $infile=$ARGV[0];
my $outfile=$ARGV[1];

open( IN, "< $infile" );
open( OUT, "> $outfile" );

while (<IN>) {

    my $line = $_;
    my $firstchar = substr ($line,0,1);
    if ( $firstchar eq '@' ) {
        my $command = substr ($line,4);
        my $commandExec = $command;
        $commandExec =~ s/[#].*$//g;  # strip off comments
        $commandExec =~ s/@@//g;   # strip off hotlink markers
        print STDERR "$commandExec";
        system "$commandExec";
        $line = $command;
        $line =~ s/@@([^@]*)@@/<a href="$1">$1<\/a>/g;
        $line =~ s/^(.*)$/    <pre><code>$1<\/code><\/pre>/g;        
        if ( $line =~ /(example\d+\.html)<\/a><\/code>/m ) {
            $line .= "\n    (View [`$1` as a web page]($1).)\n";
        }
    }
    print OUT $line;

}

