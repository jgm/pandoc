#!/bin/perl -w

$verbose = 1;
my $diffexists = `which diff`;
if ($diffexists eq "") { die "diff not found in path.\n"; }

my $script  = "COLUMNS=78 ./pandoc";

use Getopt::Long;
GetOptions("script=s" => \$script);

unless (-f $script)   { die "$script does not exist.\n"; }
unless (-x $script)   { die "$script is not executable.\n"; }

print "Writer tests:\n";

my @writeformats = ("html", "latex", "rst", "rtf", "markdown", "native"); # s5 separately
my @readformats = ("latex", "native"); # handle html,markdown & rst separately
my $source = "testsuite.native";

sub test_results 
{
    my $testname = $_[0];
    my $output = $_[1];
    my $norm = $_[2];
    my $diffoutput = `diff --strip-trailing-cr $output $norm`;
    if ($diffoutput eq "") 
    {
        print "PASSED\n";
    }
    else
    {
        print "FAILED\n";
        if ($verbose) { print $diffoutput;  } 
    } 
}

foreach my $format (@writeformats)
{
    $options = "";
    if ($format =~ /smart\./)
    {
        $options = "-S ";
    }

    my $extension = $format;
    $extension =~ s/smart\.//g;

    print "Testing $format writer...";
    `$script -r native -w $extension $options -s $source > tmp.$extension`;

    test_results("$format writer", "tmp.$extension", "writer.$format");
}

print "Testing docbook writer...";
# remove HTML block tests, as this produces invalid docbook...
`sed -e '/^, Header 1 \\[Str "HTML",Space,Str "Blocks"\\]/,/^, HorizontalRule/d' testsuite.native | $script -r native -w docbook -s > tmp.docbook`;
test_results("docbook writer", "tmp.docbook", "writer.docbook");

print "Testing s5 writer (basic)...";
`$script -r native -w s5 -s s5.native > tmp.html`;
test_results("s5 writer (basic)", "tmp.html", "s5.basic.html");

print "Testing s5 writer (fancy)...";
`$script -r native -w s5 -s -m -i s5.native > tmp.html`;
test_results("s5 writer (fancy)", "tmp.html", "s5.fancy.html");

print "Testing html fragment...";
`$script -r native -w html s5.native > tmp.html`;
test_results("html fragment", "tmp.html", "s5.fragment.html");

print "Testing -H -B -A -c options...";
`$script -r native -s -w html -H insert -B insert -A insert -c main.css s5.native > tmp.html`;
test_results("-B, -A, -H, -c options", "tmp.html", "s5.inserts.html");

print "Testing tables:\n";
print "   html writer...";
`$script -r native -w html tables.native > tmp.html`;
test_results("html table writer", "tmp.html", "tables.html");

print "   latex writer...";
`$script -r native -w latex tables.native > tmp.tex`;
test_results("latex table writer", "tmp.tex", "tables.tex");

print "   docbook writer...";
`$script -r native -w docbook tables.native > tmp.db`;
test_results("docbook table writer", "tmp.db", "tables.db");

print "   rtf writer...";
`$script -r native -w rtf tables.native > tmp.rtf`;
test_results("rtf table writer", "tmp.rtf", "tables.rtf");

print "   markdown writer...";
`$script -r native -w markdown tables.native > tmp.markdown`;
test_results("markdown table writer", "tmp.markdown", "tables.markdown");

print "   rst writer...";
`$script -r native -w rst tables.native > tmp.rst`;
test_results("rst table writer", "tmp.rst", "tables.rst");

print "   man writer...";
`$script -r native -w man tables.native > tmp.man`;
test_results("man table writer", "tmp.man", "tables.man");

print "\nReader tests:\n";

print "Testing markdown reader...";
`$script -r markdown -w native -s -S testsuite.txt > tmp.native`;
test_results("markdown reader", "tmp.native", "testsuite.native");

print "   tables...";
`$script -r markdown -w native tables.txt > tmp.native`;
test_results("markdown table reader", "tmp.native", "tables.native");

print "Testing rst reader...";
`$script -r rst -w native -s rst-reader.rst > tmp.native`;
test_results("rst reader", "tmp.native", "rst-reader.native");

print "Testing html reader...";
`$script -r html -w native -s html-reader.html > tmp.native`;
test_results("html reader", "tmp.native", "html-reader.native");

print "\nReader tests (roundtrip: X -> native -> X -> native):\n";

foreach my $format (@readformats)
{
    print "testing $format reader...";
    `$script -r $format -w native -s -R writer.$format > tmp1.native`;
    `$script -r native -w $format -s -R tmp1.native | $script -r $format -w native -s -R - > tmp2.native`;
    test_results("$format reader", "tmp1.native", "tmp2.native");
}

`rm tmp?.*`;
`rm tmp.*`;

