#!/usr/bin/perl
use ExtUtils::MakeMaker;

%req_modules = (
		'ExtUtils::MakeMaker' => '6.17',
		'Test::More' => '0.40',
		'LWP::Simple' => '1.35',
		'XML::DTDParser' => '2.00',
        'XML::LibXML' => '1.54',
	       );

WriteMakefile(
	      'NAME'         => 'XML::DT',
	      'VERSION_FROM' => 'DT.pm',
	      'EXE_FILES'    => [ "mkdtskel", "mkdtdskel", "mkxmltype" ],
	      'PM_FILTER'    => $PM_FILTER,
	      'PREREQ_PM'    => \%req_modules,
	      ($] >= 5.005 ?
	       (ABSTRACT_FROM => 'DT.pm',
	        AUTHOR        => 'Jose Joao <jj@di.uminho.pt>') : ()
	      ),
	     );
