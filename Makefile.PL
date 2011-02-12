#!/usr/bin/perl

use 5.008006;
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
	      'VERSION_FROM' => 'lib/XML/DT.pm',
	      'EXE_FILES'    => [ "mkdtskel", "mkdtdskel", "mkxmltype" ],
	      'PM_FILTER'    => $PM_FILTER,
	      'PREREQ_PM'    => \%req_modules,
              'LICENSE'		=> 'perl',
	      ($] >= 5.005 ?
	       (ABSTRACT_FROM => 'lib/XML/DT.pm',
	        AUTHOR        => 'Jose Joao <jj@di.uminho.pt>') : ()
	      ),
	     );
