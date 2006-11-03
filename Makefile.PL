use File::Copy;
use ExtUtils::MakeMaker;

my $XML = XML::LibXML;
$XML = "XML::Parser" if (grep { m!USE_XML_PARSER! } @ARGV);

print "NOTE: Now XML::DT uses by default XML::LibXML\n";
print "      Execute 'Makefile.PL USE_XML_PARSER' to for XML::Parser usage\n";
print "\n";
print "USING: $XML\n";
print "\n";
sleep 5;

$xmlparser_version = 2.30;
$xmllibxml_version = 1.54;

%req_modules = (
		'ExtUtils::MakeMaker' => '6.17',
		'Test::More' => '0.40',
		'LWP::Simple' => '1.35',
		'XML::DTDParser' => '2.00',
	       );


if ($XML eq "XML::Parser") {
  $req_modules{$XML} = $xmlparser_version;
  if ($]<5.007003) {
    # Aqui não temos Encode
    set_old_compat_func();
  } else {
    $req_modules{'Encode'} = 0;
    set_encode_func();
  }
} else {
  $XML = 'XML::LibXML';
  $req_modules{$XML} = $xmllibxml_version;
  set_libxml_func();
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
sub _fromUTF8 {
  my $string = shift;
  my $encode = shift;
  my $ans = eval { XML::LibXML::decodeFromUTF8($encode, $string) };
  if ($@) {
    return $string
  } else {
    return $ans
  }
}
EOPERL
}

sub set_old_compat_func {
  set_func(<<'EOPERL');
sub _fromUTF8 {
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
sub _fromUTF8 {
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
