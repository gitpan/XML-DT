use ExtUtils::MakeMaker;

$xmlparser_version = 2.30;
$xmllibxml_version = 1.54;

%req_modules = (
		'ExtUtils::MakeMaker' => '6.03',
		'Test::More' => '0.40',
		'LWP::Simple' => '1.35',
	       );

print "\n";
print " NOTE: Next versions will, possibly, deprecate XML::Parser backend.\n";
print "       At the moment, XML::LibXML is quicker than XML::Parser and\n";
print "       supports HTML parsing\n\n";

print "I need a XML Back-end (XML::Parser >= $xmlparser_version or XML::LibXML >= $xmllibxml_version\n";
do {
  print "Sorry, I didn't understand the answer.\n" if ($answer);
  print "Should I use 'XML::Parser' or 'XML::LibXML'? [XML::LibXML] ";
  $XML = undef;
  $answer = <STDIN>;
  print "\n";
  if ($answer =~ m/^XML::Parser$/i) {
    $XML = 'XML::Parser';
    %req_modules = (%req_modules, $XML => $xmlparser_version);
  } elsif ($answer =~ m/^\s*$/ || $answer =~ m/^XML::LibXML$/i) {
    $XML = 'XML::LibXML';
    %req_modules = (%req_modules, $XML => $xmllibxml_version);
  }
} until defined $XML;

my $pattern = uc($XML);

#### --- now, construct PM_FILTER
$PM_FILTER = qq(perl -npe \\"s/\\#$pattern\\#//;\\");
if ($^V gt v5.8.0) {
  $PM_FILTER = qq(perl -npe \"s/\\#$pattern\\#//;\");
}

WriteMakefile(
	      'NAME'         => 'XML::DT',
	      'VERSION_FROM' => 'DT.pm',
	      'EXE_FILES'    => [ "mkdtskel", "mkdtdskel" ],
	      'PM_FILTER'    => $PM_FILTER,
	      'PREREQ_PM'    => \%req_modules,
	      ($] >= 5.005 ?
	       (ABSTRACT_FROM => 'DT.pm',
	        AUTHOR        => 'Jose Joao <jj@di.uminho.pt>') : ()
	      ),
	     );
