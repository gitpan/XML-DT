# -*- cperl -*-

use Test::More tests=>1;
use XML::DT;

my $result = pathdtstring("<a><b>1</b><c>1</c><d>1</d></a>",
				('-default' => sub{"$c"},
				 'b|d' => sub{"2"}));
is($result,"212");


