# -*- cperl -*-

use Test::More tests => 10;
use XML::DT;
ok(1);

# normalize_space
is(XML::DT::normalize_space("  teste  "), "teste");
is(XML::DT::normalize_space("\tteste\t"), "teste");
is(XML::DT::normalize_space("\tteste  "), "teste");

is(XML::DT::normalize_space(" spaces   in   \t the middle\t"),
   "spaces in the middle");

# toxml as function
is(toxml("a",{},""), "<a></a>");
is(toxml("a",{},"c"), "<a>c</a>");
is(toxml("a",{a=>1},"c"), "<a a=\"1\">c</a>");

# toxml with variables
$q = a;
$c = b;
%v = ();
is(toxml, "<a>b</a>");

$v{foo} = "bar";
is(toxml, "<a foo=\"bar\">b</a>");
