## -*- cperl -*-

package XML::DT;

BEGIN{

#XML::LIBXML# use XML::LibXML;
#XML::PARSER# use XML::Parser;

use Data::Dumper;
use LWP::Simple;
use Exporter ();
use vars qw($c %v $q @dtcontext %dtcontextcount @dtatributes );
eval "use bytes";
if (my $m = $INC{"bytes.pm"}) {require bytes; import bytes;}
@ISA=qw(Exporter);
@EXPORT=qw(&dt &dtstring &dturl &inctxt &ctxt &mkdtskel &mkdtdskel &toxml 
 &MMAPON $c %v $q &xmltree &pathdturl
 @dtcontext %dtcontextcount @dtatributes &pathdt &pathdtstring );
$VERSION = '0.29';
#XML::LIBXML# $PARSER = 'XML::LibXML';
#XML::PARSER# $PARSER = 'XML::Parser';
}

=head1 NAME

XML::DT - a package for down translation of XML files

=head1 SYNOPSIS

use XML::DT;

%xml=( 'music'    => sub{"Music from: $c\n"},
   'lyrics'   => sub{"Lyrics from: $v{name}\n"},
   'title'    => sub{ uc($c) },
   '-default' => sub{"$q:$c"} );

print dt($filename,%xml);

=head1 ABSTRACT

This module is a XML down processor. It maps tag (element)
names to functions to process that element and respective
contents.

=head1 DESCRIPTION

This module processes XML files with an approach similar to
OMNIMARK. As XML parser it uses XML::Parser or XML::LibXML module in
an independent way. At configure stage, you should choose one of the
back-ends.

If you use XML::LibXML module as backend, you can parse HTML files as
if they were XML files. For this, you must supply an extra option to
the hash:

%hander = ( -html => 1,
      ...
    );

=head2 C<dt> function

Down translation function C<dt> receives a filename and a set of
expressions (functions) defining the processing and associated values
for each element.

=head2 C<dtstring> function

C<dtstring> works in a similar way with C<dt> but takes input from a
string instead of a file.

=head2 C<dturl> function

C<dturl> works in a similar way with C<dt> but takes input from an
Internet url instead of a file.

=head2 C<pathdt> function

The C<pathdt> function is a C<dt> function which can handle a subset
of XPath on handler keys. Example:

%handler = (
"article/title" => sub{ toxml("h1",{},$c) },
"section/title" => sub{ toxml("h2",{},$c) },
"title"         => sub{ $c },
"//image[@type='jpg']" => sub{ "JPEG: <img src=\"$c\">" },
"//image[@type='bmp']" => sub{ "BMP: sorry, no bitmaps on the web" },
)

pathdt($filename,%handler);

Here are some examples of valid XPath expressions under XML::DT:

/aaa
/aaa/bbb
//ccc                           - ccc somewhere (same as "ccc")
/*/aaa/*
//*                             - same as "-default"
/aaa[@id]                       - aaa with an attribute id
/*[@*]                          - root with an attribute
/aaa[not(@name)]                - aaa with no attribute "name"
//bbb[@name='foo']              - ... attribute "name" = "foo"
/ccc[normalize-space(@name)='bbb']
//*[name()='bbb']               - complex way of saying "//bbb"
//*[starts-with(name(),'aa')]   - an element named "aa.*"
//*[contains(name(),'c')]       - an element       ".*c.*"
//aaa[string-length(name())=4]                     "...."
//aaa[string-length(name())&lt;4]                  ".{1,4}"
//aaa[string-length(name())&gt;5]                  ".{5,}"

Note that not all XPath is currently handled by XML::DT. A lot of
XPath will never be added to XML::DT because is not in accordance with
the down translation model. For more documentation about XPath check
the specification at http://www.w3c.org or some tutorials under
http://www.zvon.org

=head2 C<pathdtstring> function

Like the C<dtstring> function but supporting XPath.

=head2 C<pathdturl> function

Like the C<dturl> function but supporting XPath.

=head2 C<inctxt> function

C<inctxt(pattern)> is true if the actual element path matches the
provided pattern. This function is meant to be used in the element
functions in order to achieve context dependent processing.

=head2 C<toxml> function

This is the default "-default" function. It can be used to generate
XML based on C<$c> C<$q> and C<%v> variables. Example: add a new
attribute to element C<ele1> without changing it:

%handler=( ...
     ele1 => sub { $v{at1} = "v1"; toxml(); },
   )

C<toxml> can also be used with 3 arguments: tag, attributes and contents

toxml("a",{href=> "http://local/f.html"}, "example")

returns:

<a href='http://local/f.html'>example</a>

=head2 C<xmltree> function

This simple function just makes a HASH reference:

{ -c => $c, -q => $q, all_the_other_attributes }

The function C<toxml> understands this structure and makes XML with it.




=head1 User provided element processing functions

The user must provide an HASH with a function for each element, that
computes element output. Functions can use the element name C<$q>, the
element content C<$c> and the attribute values hash C<%v>.

All those global variables are defined in C<$CALLER::>.

Each time an element is find the associated function is called.

Content is calculated by concatenation of element contents strings and
interior elements return values.

=head2 C<-default> function

When a element has no associated function, the function associated
with C<-default> called. If no C<-default> function is defined the
default function returns a XML like string for the element.

When you use C</-type> definitions, you often need do set C<-default>
function to return just the contents: C<sub{$id}>.

=head2 C<-outputenc> option

C<-outputenc> defines the output encoding (default is Unicode UTF8).

=head2 C<-inputenc> option

C<-inputenc> forces a input encoding type. Whenever that is possible,
define the input encoding in the XML file:

<?xml version='1.0' encoding='ISO-8859-1'?>

=head2 C<-pcdata> function

C<-pcdata> function is used to define transformation over the
contents.  Typically this function should look at context (see
C<inctxt> function)

The default C<-pcdata> function is the identity

=head2 C<-begin> function

Function to be executed before processing XML file.

Example of use: initialization of side-effect variables

=head2 C<-end> function

Function to be executed after processing XML file.  I can use C<$c>
content value.  The value returned by C<-end> will be the C<dt> return
value.

Example of use: post-processing of returned contents



=head1 Elements with values other than strings (C<-type>)

By default all elements return strings, and contents (C<$c>) is the
concatenation of the strings returned by the sub-elements.

In some situations the XML text contains values that are better
processed as a structured type.

The following types (functors) are available:

=over 4

=item STR

concatenates all the sub-elements returned values (DEFAULT) all the
sub-element should return strings to be concatenated;

=item SEQ

makes an ARRAY with all the sub elements contents; attributes are
ignored (they should be processed in the sub-element). (returns a ref)
If you have different types of sub-elements, you should use SEQH

=item SEQH

makes an ARRAY of HASH with all the sub elements (returns a ref); for
each sub-element:

-q  => element name
-c  => contents
at1 => at value1    for each attribute

=item MAP

makes an HASH with the sub elements; keys are the sub-element names,
values are their contents. Attributes are ignored. (they should be
processed in the sub-element) (returns a ref)

=item MULTIMAP

makes an HASH of ARRAY; keys are the sub-element names; values are
lists of contents; attributes are ignored (they should be processed in
the sub-element); (returns a ref)

=item MMAPON(element-list)

makes an HASH with the sub-elements; keys are the sub-element names,
values are their contents; attributes are ignored (they should be
processed in the sub-element); for all the elements contained in the
element-list, it is created an ARRAY with their contents. (returns a
ref)

=item XML

return a reference to an HASH with:

-q  => element name
-c  => contents
at1 => at value1    for each attribute

=item ZERO

don't process the sub-elements; return ""

=back

When you use C</-type> definitions, you often need do set C<-default>
function returning just the contents C<sub{$id}>.

=head2 An example:

use XML::DT;
%handler = ( contacts => sub{ [ split(";",$c)] },
	-default => sub{$c},
	-type    => { institution => 'MAP',
		      degrees     =>  MMAPON('name')
		      tels        => 'SEQ' }
      );
$a = dt ("f.xml", %handler);

with the following f.xml

<degrees>
<institution>
<id>U.M.</id>
<name>University of Minho</name>
<tels>
<item>1111</item>
<item>1112</item>
<item>1113</item>
</tels>
<where>Portugal</where>
<contacts>J.Joao; J.Rocha; J.Ramalho</contacts>
</institution>
<name>Computer science</name>
<name>Informatica </name>
<name> history </name>
</degrees>

would make $a

{ 'name' => [ 'Computer science',
	'Informatica ',
	' history ' ],
'institution' => { 'tels' => [ 1111,
			   1112,
			   1113 ],
	       'name' => 'University of Minho',
	       'where' => 'Portugal',
	       'id' => 'U.M.',
	       'contacts' => [ 'J.Joao',
			       ' J.Rocha',
			       ' J.Ramalho' ] } };


=head1 DT Skeleton generation

It is possible to build an initial processor program based on an example

To do this use the function C<mkdtskel(filename)>.

Example:

perl -MXML::DT -e 'mkdtskel "f.xml"' > f.pl

=head1 DTD skeleton generation

It makes a naive DTD based on an example(s).

To do this use the function C<mkdtdskel(filename*)>.

Example:

perl -MXML::DT -e 'mkdtdskel "f.xml"' > f.dtd

=head1 SEE ALSO

mkdtskel(1) and mkdtdskel(1)

=head1 AUTHORS

Home for XML::DT;

http://natura.di.uminho.pt/~jj/perl/XML/

Jose Joao Almeida, <jj@di.uminho.pt>

Alberto Manuel Simões, <albie@alfarrabio.di.uminho.pt>

thanks to

Michel Rodriguez    <mrodrigu@ieee.org>
José Carlos Ramalho <jcr@di.uminho.pt>
Mark A. Hillebrand

=cut



%ty=();

sub dt {
my ($file,%xml)=@_;
my $declr = "";
if ($xml{-declr}) {
if ($xml{-outputenc}) {
$declr = "<?xml version=\"1.0\" encoding=\"$xml{-outputenc}\"?>\n";
} else {
$declr = "<?xml version=\"1.0\"?>\n";
}
}

%ty=();
%ty=(%{$xml{'-type'}}) if defined($xml{'-type'}); 
$ty{-ROOT} = "NONE";

if ($xml{-begin}){ &{$xml{-begin}} }

#XML::LIBXML## TODO --- how to force encoding with XML::LibXML?
#XML::LIBXML## $xml{-inputenc}

#XML::LIBXML## create a new LibXML parser
#XML::LIBXML# my $parser = XML::LibXML->new();

#XML::LIBXML## Check if we should expand entities
#XML::LIBXML#  $parser->expand_entities(1) if defined $xml{'-noexpand'} && $xml{'-noexpand'};

#XML::LIBXML## parse the file
#XML::LIBXML#  my $doc;
#XML::LIBXML#  if    ( $xml{'-html'}) {
#XML::LIBXML#    eval{$doc = $parser->parse_html_file($file);};
#XML::LIBXML#    if ($@) {warn("Erro: $@\n"); } #{return undef; }
#XML::LIBXML#  }
#XML::LIBXML#  else{ $doc = $parser->parse_file($file)  }

#XML::LIBXML## get the document root element
#XML::LIBXML#  my $tree = $doc->getDocumentElement();

#XML::PARSER## create a new XML::Parser instance using Tree Style
#XML::PARSER#  if (defined($xml{-inputenc}) && ($xml{-inputenc} eq 'ISO-8859-1')){
#XML::PARSER#     $parser = new XML::Parser(Style => 'Tree',
#XML::PARSER#                               ErrorContext => 2 ,
#XML::PARSER#                               ProtocolEncoding => 'ISO-8859-1');
#XML::PARSER#  }
#XML::PARSER#  else { $parser = new XML::Parser(Style => 'Tree',
#XML::PARSER#                                   ErrorContext => 2 ,
#XML::PARSER#                                  );
#XML::PARSER#  }

#XML::PARSER## Convert XML to Perl code
#XML::PARSER#  $tree = $parser->parsefile($file);


my $return = "";
# execute End action if it exists
if($xml{-end}) {
#XML::LIBXML#    $c = omni("-ROOT", \%xml, $tree);
#XML::PARSER#    $c = omni("-ROOT", \%xml, @$tree);
$return = &{$xml{-end}}
} else {
#XML::LIBXML#    $return = omni("-ROOT",\%xml, $tree)
#XML::PARSER#    $return = omni("-ROOT",\%xml, @$tree)
}

if ($declr) {
   return $declr.$return;
} else {
   return $return;
}
}

sub ctxt {
my $level = $_[0];
$dtcontext[-$level-1];
}

sub inctxt {
my $pattern = shift ;
# see if is in root context...
return 1 if (($pattern eq "^" && @dtcontext==1) || $pattern eq ".*");
join("/",@dtcontext) =~ m!$pattern/[^/]*$! ;
}


sub pathdtstring{
my $string = shift;
my %h = pathtodt(@_);
return dtstring($string,%h);
}

sub pathdturl{
my $url = shift;
my %h = pathtodt(@_);
return dturl($url,%h);
}

sub dturl{
my $url = shift;
my $contents = get($url);
if ($contents) {
return dtstring($contents, @_);
} else {
return undef;
}
}

sub dtstring {
my ($string,%xml)=@_;

my $declr = "";
if ($xml{-declr}) {
if ($xml{-outputenc}) {
$declr = "<?xml version=\"1.0\" encoding=\"$xml{-outputenc}\"?>\n";
} else {
$declr = "<?xml version=\"1.0\"?>\n";
}
}

$xml{'-type'} = {} unless defined $xml{'-type'};
%ty=(%{$xml{'-type'}}, -ROOT => "NONE");

# execute Begin action if it exists
if ($xml{-begin}){ &{$xml{-begin}} }

#XML::LIBXML## TODO --- how to force encoding with XML::LibXML?
#XML::LIBXML## $xml{-inputenc}
#XML::LIBXML# $string = XML::LibXML::encodeToUTF8($xml{-inputenc},$string) if ($xml{-inputenc});

#XML::LIBXML## create a new LibXML parser
#XML::LIBXML#  my $parser = XML::LibXML->new();

#XML::LIBXML## Check if we should expand entities
#XML::LIBXML#  $parser->expand_entities(0) if defined $xml{'-noexpand'} && $xml{'-noexpand'};

#XML::LIBXML## parse the string
#XML::LIBXML#  my $doc;

#XML::LIBXML#  $doc = $parser->parse_string($string)      unless( $xml{'-html'});
#XML::LIBXML#  if    ( $xml{'-html'}) {
#XML::LIBXML#    eval{$doc = $parser->parse_html_string($string);};
#XML::LIBXML#    if ($@) { return undef; }
#XML::LIBXML#  }



#XML::LIBXML## get the document root element
#XML::LIBXML#  my $tree = $doc->getDocumentElement();

#XML::PARSER## create a new XML::Parser instance using Tree Style
#XML::PARSER#  if (defined($xml{-inputenc}) && ($xml{-inputenc} eq 'ISO-8859-1')){
#XML::PARSER#     $parser = new XML::Parser(Style => 'Tree',
#XML::PARSER#                               ErrorContext => 2 ,
#XML::PARSER#                               ProtocolEncoding => 'ISO-8859-1');
#XML::PARSER#  }
#XML::PARSER#  else { $parser = new XML::Parser(Style => 'Tree',
#XML::PARSER#                                   ErrorContext => 2 ,
#XML::PARSER#                                  );
#XML::PARSER#  }

#XML::PARSER## Convert XML to Perl code (Tree)
#XML::PARSER#  $tree = $parser->parse($string);

my $return;

# Check if we have an end function
if ($xml{-end}) {
#XML::LIBXML#    $c = omni("-ROOT", \%xml, $tree);
#XML::PARSER#    $c = omni("-ROOT", \%xml, @$tree);
$return = &{$xml{-end}}
} else {
#XML::LIBXML#    $return = omni("-ROOT", \%xml, $tree)
#XML::PARSER#    $return = omni("-ROOT", \%xml, @$tree)
}

if ($declr) {
   return $declr.$return;
} else {
   return $return;
}
}

sub pathdt{
my $file = shift;
my %h = pathtodt(@_);
return dt($file,%h);
}

# Parsing dos predicados do XPath
sub testAttr {
my $atr = shift;
for ($atr) {
s/name\(\)/'$q'/g;
# s/\@([A-Za-z_]+)/'$v{$1}'/g;
s/\@([A-Za-z_]+)/defined $v{$1}?"'$v{$1}'":"''"/ge;
s/\@\*/keys %v?"'1'":"''"/ge;
if (/^not\((.*)\)$/) {
return ! testAttr($1);
} elsif (/^('|")([^\1]*)(\1)\s*=\s*('|")([^\4]*)\4$/) {
return ($2 eq $5);
} elsif (/normalize-space\((['"])([^\1)]*)\1\)/) {
my ($back,$forward)=($`,$');
my $x = normalize_space($2);
return testAttr("$back'$x'$forward"); 
} elsif (/starts-with\((['"])([^\1))]*)\1,(['"])([^\3))]*)\3\)/) {
my $x = starts_with($2,$4);
return $x;
} elsif (/contains\((['"])([^\1))]*)\1,(['"])([^\3))]*)\3\)/) {
my $x = contains($2,$4);
return $x; 
} elsif (/string-length\((['"])([^\1]*)\1\)/) {
my ($back,$forward) = ($`,$');
my $x = length($2);
return testAttr("$back$x$forward");
} elsif (/^(\d+)\s*=(\d+)$/) {
return ($1 == $2);
} elsif (/^(\d+)\s*&lt;(\d+)$/) {
return ($1 < $2);
} elsif (/^(\d+)\s*&gt;(\d+)$/) {
return ($1 > $2);
} elsif (/^(['"])([^\1]*)\1$/) {
return $2;
}
}
return 0; #$atr;
}

# Funcao auxiliar de teste de predicados do XPath
sub starts_with {
my ($string,$preffix) = @_;
return 0 unless ($string && $preffix);
return 1 if ($string =~ m!^$preffix!);
return 0;
}

# Funcao auxiliar de teste de predicados do XPath
sub contains {
my ($string,$s) = @_;
return 0 unless ($string && $s);
return 1 if ($string =~ m!$s!);
return 0;
}

# Funcao auxiliar de teste de predicados do XPath
sub normalize_space {
my $z = shift;
$z =~ /^\s*(.*?)\s*$/;
$z = $1;
$z =~ s!\s+! !g;
return $z;
}

sub pathtodt {
my %h = @_;
my %aux=();
my %aux2=();
my %n = ();
my $z;
for $z (keys %h) {
# TODO - Make it more generic
if ( $z=~m{\w+(\|\w+)+}) {
	my @tags = split /\|/, $z;
	for(@tags) {
		$aux2{$_}=$h{$z}
	}
}
elsif ( $z=~m{(//|/|)(.*)/([^\[]*)(?:\[(.*)\])?} ) {
my ($first,$second,$third,$fourth) = ($1,$2,$3,$4);
if (($first eq "/") && (!$second)) {
$first = "";
$second = '.*';
$third =~ s!\*!-default!;
} else {
$second =~ s!\*!\[^/\]\+!g;
$second =~ s!/$!\(/\.\*\)\?!g;
$second =~ s!//!\(/\.\*\)\?/!g;
$third =~ s!\*!-default!g;
}
push( @{$aux{$third}} , [$first,$second,$h{$z},$fourth]);
}
else                           { $aux2{$z}=$h{$z};}
}
for $z (keys %aux){
my $code = sub {
 my $l;
 for $l (@{$aux{$z}}) {
    my $prefix = "";
    $prefix = "^" unless (($l->[0]) or ($l->[1]));
    $prefix = "^" if (($l->[0] eq "/") && ($l->[1]));
    if ($l->[3]) {
	if(inctxt("$prefix$l->[1]") && testAttr($l->[3])) 
	  {return &{$l->[2]}; }
    } else {
	if(inctxt("$prefix$l->[1]")) {return &{$l->[2]};}
    }
 }
return &{ $aux2{$z}} if $aux2{$z} ;
return &{ $h{-default}} if $h{-default};
&toxml();
};
$n{$z} = $code;
}
for $z (keys %aux2){
$n{$z} ||= $aux2{$z} ;
}
return %n;
}

sub omni{
my ($par, $xml, @l) = @_;
my $type = $ty{$par} || "STR";
my %typeargs = ();

if (ref($type) eq "mmapon") {
for (@$type)
{ $typeargs{$_} = 1 }
$type = "MMAPON";
};

my $r ;

if( $type eq 'STR')                                 { $r = "" }
elsif( $type eq 'SEQ'  or $type eq "ARRAY")         { $r = [] }
elsif( $type eq 'SEQH' or $type eq "ARRAYOFHASH")   { $r = [] }
elsif( $type eq 'MAP'  or $type eq "HASH")          { $r = {} }
elsif( $type eq 'MULTIMAP')                         { $r = {} }
elsif( $type eq 'MMAPON' or $type eq "HASHOFARRAY") { $r = {} }
elsif( $type eq 'NONE')                             { $r = "" }
elsif( $type eq 'ZERO')                             { return "" }

my ($name, $val, @val, $atr, $aux);

while(@l) {
#XML::LIBXML#    my $tree = shift @l;
#XML::LIBXML#    if (ref($tree) eq "XML::LibXML::CDATASection") {
#XML::LIBXML#      $name = "-pcdata";
#XML::LIBXML#      $val = $tree->getData();
#XML::LIBXML#      $aux= (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1')
#XML::LIBXML#	            ?lat1::utf8($val): $val ;
#XML::LIBXML#      if (defined($xml->{-pcdata})) {
#XML::LIBXML#	      push(@dtcontext,"-pcdata");
#XML::LIBXML#	      $c = $aux;
#XML::LIBXML#	      $aux = &{$xml->{-pcdata}};
#XML::LIBXML#	      pop(@dtcontext);
#XML::LIBXML#      }
#XML::LIBXML#    } else {
#XML::LIBXML#      $name = $tree->getName();
#XML::LIBXML#    }
#XML::PARSER#    ($name,$val,@l) = @l;

#XML::LIBXML#    if (ref($tree) eq "XML::LibXML::Comment") {

#XML::LIBXML#      ### At the moment, treat as Text
#XML::LIBXML#      ### We will need to change this, I hope!
#XML::LIBXML#      $val = "";
#XML::LIBXML#      $name = "-pcdata";
#XML::LIBXML#      $aux= (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1')
#XML::LIBXML#	?lat1::utf8($val): $val ;
#XML::LIBXML#      if (defined($xml->{-pcdata})) {
#XML::LIBXML#	push(@dtcontext,"-pcdata");
#XML::LIBXML#	$c = $aux;
#XML::LIBXML#	$aux = &{$xml->{-pcdata}};
#XML::LIBXML#	pop(@dtcontext);
#XML::LIBXML#      }

#XML::LIBXML#    } elsif (ref($tree) eq "XML::LibXML::Text") {
#XML::PARSER#    if ($name eq "0") {
#XML::LIBXML#      $val = $tree->getData();

$name = "-pcdata";
#XML::PARSER# $aux= (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1')
#XML::PARSER#  ?lat1::utf8($val): $val ;

#XML::LIBXML# $aux= (defined($xml->{-outputenc}))
#XML::LIBXML#  ?decodeFromUTF8($xml->{-outputenc},$val): $val ;
if (defined($xml->{-pcdata})) {
push(@dtcontext,"-pcdata");
$c = $aux;
$aux = &{$xml->{-pcdata}};
pop(@dtcontext);
}
} else {
#XML::LIBXML#      my %atr = nodeAttributes($tree);
#XML::LIBXML#      $atr = \%atr;
#XML::PARSER#      ($atr,@val) = @$val;

push(@dtcontext,$name); $dtcontextcount{$name}++;
unshift(@dtatributes, $atr);
#XML::LIBXML#      $aux = omniele($xml, $name, omni($name, $xml, ($tree->getChildnodes())), $atr);
#XML::PARSER#      $aux = omniele($xml, $name, omni($name,$xml,@val), $atr);
shift(@dtatributes);
pop(@dtcontext); $dtcontextcount{$name}--;
}
if   ($type eq "STR"){ if (defined($aux)) {$r .= $aux} ;}
elsif($type eq "SEQ" or $type eq "ARRAY"){
push(@$r, $aux) unless whitepc($aux, $name);}
elsif($type eq "SEQH" or $type eq "ARRAYHASH"){
push(@$r,{"-c" => $aux,
	"-q" => $name,
#XML::LIBXML#		nodeAttributes($tree) }) unless whitepc($aux,$name);}
#XML::PARSER#                    %$atr }) unless whitepc($aux,$name);}
elsif($type eq "MMAPON"){
if(not whitepc($aux,$name)){
if(! $typeargs{$name}) {
  warn "duplicated tag ´$name´\n" if(defined($r->{$name}));
  $r->{$name} = $aux }
else { push(@{$r->{$name}},$aux) unless whitepc($aux,$name)}}
}
elsif($type eq "MAP" or $type eq "HASH"){
if(not whitepc($aux,$name)){
warn "duplicated tag ´$name´\n" if(defined($r->{$name}));
$r->{$name} = $aux }}
elsif($type eq "MULTIMAP"){
push(@{$r->{$name}},$aux) unless whitepc($aux,$name)}
elsif($type eq "NONE"){ $r = $aux;}
else { $r="undefined type !!!"}
}
$r;

}

sub omniele {
my $xml = shift;
my $aux;
($q, $c, $aux) = @_;

%v = %$aux;

if (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1'){
for (keys %v){ $v{$_} = lat1::utf8($v{$_}) ; }
}

if    (defined $xml->{$q}) {&{$xml->{$q}} }
elsif (defined $xml->{'-default'}) {&{$xml->{'-default'} }}
else  {toxml();}
}

sub xmltree {
+{'-c' => $c, '-q' => $q, %v}
}

sub toxmlp {
my ($q,$v,$c ) = @_;
if ($q eq "-pcdata") { $c }
else {"<$q" . join("",map {" $_=\"$v->{$_}\""} keys %$v ) . ">$c</$q>" }
}

sub toxml {
my ($q,$v,$c);

if (not @_) {
($q,$v,$c) = ($XML::DT::q, \%XML::DT::v, $XML::DT::c);
} elsif (ref($_[0])) {
$c = shift;
} else {
($q,$v,$c) = @_;
}

if (not ref($c)) {
if ($q eq "-pcdata") {
return $c
}
return openTag($q,$v) . "$c</$q>"
}
elsif (ref($c) eq "HASH" && $c->{'-q'} && $c->{'-c'}) {
my %a = %$c;
my ($q,$c) = delete @a{"-q","-c"};
toxml($q,\%a,$c);
}
elsif (ref($c) eq "HASH") {
openTag($q,$v).
join("",map {($_ ne "-pcdata")
	     ? ( (ref($c->{$_}) eq "ARRAY")
		 ? "<$_>".
		 join("</$_>\n<$_>", @{$c->{$_}}).
		 "</$_>\n" 
		 : toxml($_,{},$c->{$_})."\n" )
	       : () }
   keys %{$c} ) .
     "$c->{-pcdata}</$q>" } ########  "NOTYetREady"
elsif (ref($c) eq "ARRAY") {
if($ty{$q} eq "SEQH"){toxml($q,$v,join("\n",map {toxml($_)} @$c))}
else                 {toxml($q,\%v, join("",@{$c}))}
}
}

sub openTag{
"<$_[0]". join("",map {" $_=\"$_[1]{$_}\""} keys %{$_[1]} ).">"
}

sub toxml1 {
if(@_ == 3){return toxmlp(@_)}
return "" if (defined $ty{$q} && $ty{$q} eq "ZERO");
if(not ref($c)){  toxmlp($q,\%v,$c)}
elsif (ref($c) eq "ARRAY") {
if($ty{$q} eq "SEQH") {
toxmlp($q,{},
     join("",map {my %a=%$_; 
		  delete @a{"-q","-c"}; 
		  toxmlp($_->{'-q'},\%a,$_->{-c}) } @{$c} ))
}
else { toxmlp($q,\%v, join("",@{$c}))}
}
elsif (ref($c) eq "HASH") {
"<$q".
join("",map {" $_=\"$v{$_}\""} keys %v ) . ">" .
join("",map {($_ ne "-pcdata")
	       ? ( (ref($c->{$_}) eq "ARRAY")
		   ? "<$_>".
		   join("</$_>\n<$_>", @{$c->{$_}}).
		   "</$_>\n"
		   : "<$_>$c->{$_}</$_>\n" )
		 : () }
     keys %{$c} ) .
       "$c->{-pcdata}</$q>" }
}

sub mkdtskel{
my @files = @_;
my %mkdtskel =
(

'-default' => sub{
$element{$q}++;
for (keys %v) {
 $att{$q}{$_} = 1
};
""},

'-end' => sub{
print <<'END';
#!/usr/bin/perl
use XML::DT ;
my $filename = shift;

%handler=(
#    '-outputenc' => 'ISO-8859-1',
#    '-default'   => sub{"<$q>$c</$q>"},
END
for $name (sort keys %element) {
 print "     '$name' => sub{\n";
 print( '       # remember attributes $v{', 
	join('},$v{',keys %{$att{$name}}), "}\n")  if ($att{$name});
 print "       # occurred $element{$name} times\n";
 print "       \"\$q:\$c\"\n";
 print "     },\n";
}
print <<'END';
);
print dt($filename,%handler);
END
		       }
	 );

$file=shift(@files);
while($file =~ /^-/){
if   ($file eq "-html")   { $mkdtskel{'-html'} = 1;} 
elsif($file eq "-latin1") { $mkdtskel{'-inputenc'}='ISO-8859-1';}
else { die("usage mktskel [-html] [-latin1] file \n")}
$file=shift(@files)}

dt($file,%mkdtskel)
}

sub nodeAttributes {
  my $node = shift;
  my %answer = ();
  my @attrs = $node->getAttributes();
  for (@attrs) {
    if (ref($_) eq "XML::LibXML::Namespace") {
      # TODO: This should not be ignored, I think.
      # This sould be converted on a standard attribute with
      # key 'namespace' and respective contents
    } else {
      $answer{$_->getName()} = $_->getValue();
    }
  }
  return %answer;
}

sub mkdtdskel {
  my @files = @_;
  my %handler=(
    '-outputenc' => 'ISO-8859-1',
    '-default'   => sub{ 
          $elel{$q}++;
          $root = $q unless ctxt(1);
          $ele{ctxt(1)}{$q} ++;
          for(keys(%v)){$att{$q}{$_} ++ } ;
        },
    '-pcdata'    => sub{ if ($c =~ /[^ \t\n]/){ $ele{ctxt(1)}{"#PCDATA"}=1 }},
  );

  while($files[0] =~ /^-/){
    if   ($files[0] eq "-html")   { $handler{'-html'} = 1;} 
    elsif($files[0] eq "-latin1") { $handler{'-inputenc'}='ISO-8859-1';}
    else { die("usage mkdtdskel [-html] [-latin1] file* \n")}
    shift(@files)}

  for $filename (@files){
  dt($filename,%handler); 
  }

  print "<!-- DTD $root ... -->\n<!-- (C) ... " . localtime(time) ." -->\n";
  delete $elel{$root};

  for ($root, keys %elel){
    putele($_);
    for $name (keys(%{$att{$_}})) {
       print( "\t<!-- $name : ... -->\n");
       print( "\t<!ATTLIST $_ $name CDATA #IMPLIED >\n");
    }
  }
}


sub process_libxml {
  my $node = shift;
  my $ref = ref($node);
  if ($ref eq "XML::LibXML::Text") {
    return [0, $node->toString() ];
  } else {
    my $name = $node->getName();
    my %attr = ( map { ($_->getName(), $_->getValue()) } $node->getAttributes() );

    my $x = [ $name, [ { %attr }, map {my $w =  process_libxml($_); @$w } $node->getChildnodes()  ]];
    return $x;
  }
}



sub putele {
  my $e = shift;
  my @f ;
  if ($ele{$e}) {
    @f = keys %{$ele{$e}};
    print "<!ELEMENT $e (", join("|", @f ),")",
      (@f >= 1 && $f[0] eq "#PCDATA" ? "" : "*"),
	" >\n";
    print "<!-- ", join(" | ", (map {"$_=$ele{$e}{$_}"} @f )), " -->\n";
  }
  else {
    print "<!ELEMENT $e  EMPTY >\n";
  }
}

sub whitepc {
  $_[1] eq '-pcdata' and $_[0] =~ /^[ \t\n]*$/
}

sub MMAPON {
  bless([@_],"mmapon")
}

sub SEQOF {
  bless([@_],"seqof")
}

package lat1;

=head1 NAME

C<lat1.pm> - module for unicode utf8 to latin1 translation

=head1 SYNOPSIS

   $latin1string = lat1::utf8($utf8string)

=head1 Bugs

Translating the latin1 subset of unicode utf8 is very simples and needs no
tables.

If you need more complex translation, see the perl modules about unicode
and the C<recode> command.

=cut

sub utf8{
  my $t=shift;
  $t =~ s/([ÃÂ])(.)/ $1 eq "Ã" ? chr( ord($2) | 0100): $2 /ge;
  $t;
}

#sub utf8{
#  my $t=shift;
#  if($] >= 5.006){$t =~ tr/\200-\377//UC;}
#  else           {$t =~ s/([ÃÂ])(.)/ $1 eq "Ã" ? chr( ord($2) | 0100): $2 /ge;}
#  $t;
#}

1;
