use ExtUtils::MakeMaker;

$xmlparser_version = v2.30;
$xmllibxml_version = v1.31;


print "I need a XML Back-end (XML::Parser ",
  sprintf("v%vd",$xmlparser_version), " or XML::LibXML ",
  sprintf("v%vd",$xmllibxml_version),"\n";
do {
  print "Sorry, I didn't understand the answer.\n" if ($answer);
  print "Should I use 'XML::Parser' or 'XML::LibXML'? [XML::LibXML] ";
  $XML = undef;
  $answer = <>;
  print "\n";
  if ($answer =~ /^XML::Parser$/i) {
    $XML = 'XML::Parser';
    check_module('XML::Parser', $xmlparser_version);
  } elsif ($answer =~ /^$/ || $answer =~ /^XML::LibXML$/i) {
    $XML = 'XML::LibXML';
    check_module('XML::LibXML', $xmllibxml_version);
  }
} while (not defined($XML));

check_module('Test::More',  v0.40);

####
my $pattern = uc($XML);
`cat DT.pm.in | perl -e 'while(<>) { s/#$pattern#//; print }' > DT.pm`;
#####

WriteMakefile(
	      'NAME'         => 'XML::DT',
	      'VERSION_FROM' => 'DT.pm',
	      'clean'        => { FILES => "DT.pm"},
	      'EXE_FILES'    => [ "mkdtskel" ],
);

sub MY::postamble {
 "
DT.pm: DT.pm.in
\t`cat DT.pm.in | perl -e 'while(<>) { s/#$pattern#//; print }' > DT.pm`
"
}



### Aux function
sub check_module { # (module name, version)
  my $module = shift;
  my $version = shift || "?";

  if ($version eq "?") {
    print "Checking for $module existence... ";
    my $command = " use $module; ";
    my $answer = eval($command);
    if ($@) {
      print "not found\n";
      exit
    } else {
      print "yes\n";
    }
  } else {
    print "Checking for $module >= ",sprintf("%vd",$version),"... ";
    my $command = " use $module; return \$${module}::VERSION;";
    my $answer = eval ( $command );
    if ($@) {
      print "not found\n";
      exit
    } else {
      if (defined($answer)) {
	#this line converts a number to a 'v number'
	#TODO: this should not be done if it is already a 'v number'
	my $t = "v$answer"; $answer = eval($t);

	if ($answer ge $version) {
	  print sprintf("yes (%vd)\n", $answer);
	} else {
	  print sprintf("no (%vd)\n", $answer);
	  exit
	}
      } else {
	print "?? (this module does not define \$VERSION variable)\n";
      }
    }
  }
}

