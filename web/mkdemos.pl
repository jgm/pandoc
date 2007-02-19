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
        print STDERR "$command";
        system "$command";
        $line = $command;
        $line =~ s/-/\-/;
        $line =~ s/ ([A-Za-z0-9_:\/]+(\.|\/)[a-zA-Z0-9.\/]*|README|S5DEMO)/ <a href="$1">$1<\/a>/g;
        $line =~ s/-/\\-/g;
        $line =~ s/^(.*)$/    <code>$1<\/code>/g;        
        if ( $line =~ /(example\d+\.html)<\/a><\/code>/m ) {
            $line .= "\n    (View [`$1` as a web page]($1).)\n";
        }
    }
    print OUT $line;

}

