#######################################################
#I am using XML::DT as below (the require is for selective module loading):
#######################################################
#
#require XML::DT; XML::DT->import ();
# Yes !!!

use strict;

use Data::Dumper;
use XML::DT;

my @order=qw(volume issue doi author f_page l_page artid epub ppub type);

my $M;

my %handler = (
   '-default'   => sub {$c},
   'article' => sub {
           $v{issue} = $dtattributes[1]->{number};
           $v{volume} = $dtattributes[2]->{number};

           $M .= join("\t", @v{(@order)}) . "\n";
    },
 );

 dt ("ex11.5.xml", %handler);
 print  $M;
