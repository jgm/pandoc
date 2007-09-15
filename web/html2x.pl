#!/usr/bin/env perl
use CGI qw/:standard/;
use CGI::Carp 'fatalsToBrowser';

$CGI::POST_MAX=1024 * 100;  # max 100K posts
$CGI::DISABLE_UPLOADS = 1;  # no uploads
      
if (param('url') && param('format')) {
    $options = '--standalone --reference-links';	
    $url = param('url');
    $format = param('format') || 'markdown';
    if ($format =~ '^markdown$') {
      $options .= ' --strict';
    }
    if ($format =~ '^markdown\+$') {
      $format = 'markdown';
    }
    $output = `wget -O- $url | tidy -asxhtml -utf8 | pandoc -r html -w $format $options`;
    if ($format =~ "rtf") { 
      $type = "application/rtf" 
    } else { 
      $type = "text/plain"
    }; 
	print header(-charset=>"utf8",-type=>"$type"),
	      $output;
} else {
    print start_html(-title=>"html2x"),
          h1("Usage"),
          p("You have tried to call html2x.pl without the proper parameters."),
          p("Please use <a href=\"/pandoc/html2x.html\">this form</a>."),
          end_html();
}


