use File::Copy;
#BEGIN {
#  my $PARSER;
#  $PARSER = grep { m!^PARSER=! } @ARGV;
#  @ARGV = grep { $_ ne $PARSER } @ARGV;
#}
use ExtUtils::MakeMaker;

my $PARSER;

$xmlparser_version = 2.30;
$xmllibxml_version = 1.54;

%req_modules = (
		'ExtUtils::MakeMaker' => '6.17',
		'Test::More' => '0.40',
		'LWP::Simple' => '1.35',
	       );

if ($PARSER) {
  $PARSER=~m!PARSER=!;
  $XML = uc($');
  unless ($XML eq "XML::PARSER" || $XML eq "XML::LIBXML") {
    print STDERR "Unknown parser. Using default: XML::LibXML\n";
    $XML = "XML::LIBXML";
  }
} else {
  print "\n";
  print " NOTE: Next versions will, possibly, deprecate XML::Parser backend.\n";
  print "       At the moment, XML::LibXML is quicker than XML::Parser and\n";
  print "       supports HTML parsing\n\n";

  print "I need a XML Back-end (XML::Parser >= $xmlparser_version or XML::LibXML >= $xmllibxml_version\n";
  do {
    print "Sorry, I didn't understand the answer.\n" if $answer;
    print "Should I use 'XML::Parser' or 'XML::LibXML'? [XML::LibXML] ";
    $XML = undef;
    $answer = <STDIN>;
    print "\n";
    if ($answer =~ m/^XML::Parser$/i) {
      $XML = 'XML::Parser';
      $req_modules{$XML} = $xmlparser_version;
      if ($]<5.007003) {
	# Aqui não temos Encode
	set_old_compat_func();
      } else {
	$req_modules{'Encode'} = 0;
	set_encode_func();
      }
    } elsif ($answer =~ m/^\s*$/ || $answer =~ m/^XML::LibXML$/i) {
      $XML = 'XML::LibXML';
      $req_modules{$XML} = $xmllibxml_version;
      set_libxml_func();
    }
  } until defined $XML;
}
my $pattern = uc($XML);
$PM_FILTER = qq(perl -npe \"s/\\#$pattern\\#//;\");

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

sub set_libxml_func {
  set_func(<<'EOPERL');
sub fromUTF8 {
  my $string = shift;
  my $encode = shift;
  XML::LibXML::decodeFromUTF8($encode, $string);
}
EOPERL
}

sub set_old_compat_func {
  set_func(<<'EOPERL');
sub fromUTF8 {
  my $string = shift;
  my $encode = shift; #ignoring
  {
    use bytes;
    $string =~ s/([ÃÂ])(.)/ $1 eq "Ã" ? chr( ord($2) | 0100): $2 /ge;
    $string
  }
}
EOPERL
}

sub set_encode_func {
  set_func(<<'EOPERL');
sub fromUTF8 {
  use Encode;
  my $string = shift;
  my $encode = shift;
  encode($encode, $string);
}
EOPERL
}

sub set_func {
  my $func = shift;
  open DT, "DT.pm" or die "$!\n";
  open OUT, ">DT.pmx" or die "$!\n";
  while(<DT>) {
    print OUT;
    if (m!\#\#\# STARTLATIN1!) {
      print OUT $func;
      while(<DT>) {
	last if m!\#\#\# ENDLATIN1!;
      }
      print OUT
    }
  }
  close OUT;
  close DT;
  move("DT.pmx", "DT.pm");
}
