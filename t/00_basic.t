# -*- cperl -*-

use Test::More tests => 12;
use XML::DT;
ok(1);

# normalize_space
is(XML::DT::_normalize_space("  teste  "), "teste");
is(XML::DT::_normalize_space("\tteste\t"), "teste");
is(XML::DT::_normalize_space("\tteste  "), "teste");

is(XML::DT::_normalize_space(" spaces   in   \t the middle\t"),
   "spaces in the middle");

# toxml as function
is(toxml("a",{},""), "<a/>");
is(toxml("a",{},"c"), "<a>c</a>");
is(toxml("a",{a=>1},"c"), "<a a=\"1\">c</a>");

# this is one of the most important tests for MathML
is(toxml("foo",{},"0"), "<foo>0</foo>");

# toxml with variables
$q = "a";
$c = "b";
%v = ();
is(toxml, "<a>b</a>");

$v{foo} = "bar";
is(toxml, "<a foo=\"bar\">b</a>");

$c = '0';
is(toxml, "<a foo=\"bar\">0</a>");
