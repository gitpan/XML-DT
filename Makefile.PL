use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

@extras= (ABSTRACT_FROM => 'DT.pm',
          AUTHOR        => 'Jose Joao (jj@di.uminho.pt)');

WriteMakefile(
    'NAME'         => 'XML::DT',
    dist           => {COMPRESS => 'gzip', SUFFIX => '.gz'},
    'VERSION_FROM' => 'DT.pm', # finds $VERSION
    'PREREQ_PM'    => { 'XML::Parser' => 2.15, },

    @extras
);
