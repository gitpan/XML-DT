package XML::DT;

BEGIN{
 use XML::Parser;
 use Exporter ();
 use vars qw($c %v $q @dtcontext @dtatributes );
 @ISA=qw(Exporter);
 @EXPORT=qw(dt dtstring inctxt ctxt mkdtskel mkdtdskel toxml MMAPON $c %v $q 
         @dtcontext @dtatributes );
 $VERSION = '0.14';
}

=head1 NAME

    XML::DT - a package for down translation of XML to strings

=head1 SYNOPSIS

    use XML::DT;

    %xml=( 'music'    => sub{"Music from: $c\n"},
           'lyrics'   => sub{"Lyrics from:$c\n (the value of attribute
                               IN is:$v{IN}\n)"},
           'title'    => sub{ uc($c) },
           '-default' => sub{"$q:$c"},
           '-outputenc' => 'ISO-8859-1');
    
    print dt($filename,%xml);

    print dtstring("<arq>
                    <title>Vejam Bem</title>
                    <music>Zeca Afonso</music>
                    </arq>",%xml);

    inctxt('music/lyrics')
    inctxt('music.*')

    ctxt(1)       /* the father element */

    mkdtskel($file)
    mkdtdskel($file)

=head1 DESCRIPTION

This module processes XML files with an approach similar to OMNIMARK.

Down translation function C<dt> receives a filename and a set of expressions
(functions) defining the processing and associated values for each element.

C<dtstring> is similar but takes input from a string instead of a file.

=head2 C<inctxt> function

C<inctxt(pattern)> is true if the actual element path matches the provided 
pattern. This function is ment to be used in the element functions in order
to achive context dependent processing. 

=head2 User provided element processing functions

The user must provide an HASH with a function for each element,
that computes element output. Functions can use the element name C<$q>, 
the element content C<$c> and the atribute values hash C<%v>. 

All those global variables are defined in C<$CALLER::>.

Each time an element is find the associated function is called.

Content is calculated by concatenation of element contents strings and
interior elements return values.

=head2 C<-default> function

When a element has no associated function, the function associated with 
C<-default> called. If no C<-default> function is defined the default function 
returns a XML like string for the elemente.

=head2 C<-outputenc> option

C<-outputenc> defines the output encoding (default is Unicode UTF8).

=head2 C<-inputenc> option

C<-inputenc> forces a input encoding type. Whenever that is possible,
define the input encoding in the XML file:

   <?xml version='1.0' encoding='ISO-8859-1'?>

=head2 C<-pcdata> function

C<-pcdata> function is used to define tranformation over the contents.
Typically this function should look at context (see C<inctxt> function)

The default C<-pcdata> function is the identity

=head2 C<-begin> function

Function to be executed before processing XML file.

Example of use: inicialization of sife-effect variables

=head2 C<-end> function

Function to be executed after processing XML file.
I can use C<$c> content value.
The value returned by C<-end> will be the C<dt> return value.

Example of use: post-processing of returned contents 

=head2 C<toxml> function

This is the default "-default" function. It can be used to generate xml
based on C<$c> C<$q> and C<%v> variables. Example: add a new attribute to
element C<ele1> without changing it:

  %handler=( ...
             ele1 => sub { $v{at1} = "v1"; toxml(); },
           )

=head1 Elements with values other than strings (C<-type))

By default all elements return strings, and contents (C<$c>) is the
concatenation of the strings returned by the sub-elements.

In some situations the XML text contains values that are better processed as
a structured type.

The following types (functors) are available:

     STR  -> concatenates all the subelements returned values (DEFAULT)
          all the subelement sould return strings to be concatenated
     SEQ  -> makes an ARRAY with all the sub elements contents; attritutes are
          ignored (they should be processed in the subelement). (returns a ref)
     SEQH -> makes an ARRAY of HASH with all the sub elements (returns a ref);
          for each subelement: 
                 -q  => element name
                 -c  => contents
                 at1 => at value1    for each atribute
     MAP  -> makes an HASH with the sub elements; keys are the sub-element
          names, values are their contents. Atributes are ignored. (they should
          be processed in the subelement) (returns a ref)
     MULTIMAP -> makes an HASH of ARRAY; keys are the sub-element names;
         values are lists of contents; atributes are ignored (they should be
         processed in the subelement); (returns a ref)
     MMAPON(elementlist) -> makes an HASH with the subelements; 
          keys are the sub-element names, values are their contents; 
          atributes are ignored (they should be processed in the subelement);
          for all the elements contained in the elementelist, it is created 
          an ARRAY whith their contents. (returns a ref)

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
     <name>informatic </name>
     <name> history </name>
    </degrees>

would make $a

  { 'name' => [ 'Computer science',
                'informatic ',
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

It makes a naife DTD based on an example(s).

To do this use the function C<mkdtdskel(filename*)>.

Example:

  perl -MXML::DT -e 'mkdtdskel "f.xml"' > f.dtd

=head1 BUGS

This section is out of date...

=head1 Author

Jose Joao, jj@di.uminho.pt

  http://www.di.uminho.pt/~jj/perl/XML/

thanks to 

  Michel Rodriguez <mrodrigu@ieee.org>
  José Carlos Ramanlho <jcr@di.uminho.py>

=cut

%ty=();

sub dt {
  my ($file,%xml)=@_;

  %ty=();
  %ty=(%{$xml{'-type'}}) if defined($xml{'-type'}); 
  $ty{-ROOT} = "NONE";

  # create a new XML::Parser instance using Tree Style
  if (defined($xml{-inputenc}) && ($xml{-inputenc} eq 'ISO-8859-1')){
     $parser = new XML::Parser(Style => 'Tree',
                               ErrorContext => 2 ,
                               ProtocolEncoding => 'ISO-8859-1');
  }
  else { $parser = new XML::Parser(Style => 'Tree',
                                   ErrorContext => 2 ,
                                  );
  }

  #execute Begin action if she exists
  if ($xml{-begin}){ &{$xml{-begin}} }

  # Convert XML to Perl code
  $tree = $parser->parsefile($file);

  if($xml{-end}){ $c= omni("-ROOT",\%xml,@$tree); 
                  &{$xml{-end}} }
  else          { omni("-ROOT",\%xml,@$tree) }
}

sub ctxt {
  my $level = $_[0];
  $dtcontext[-$level-1];
}

sub inctxt {
  my $pattern = shift ;
  join("/",@dtcontext) =~ m!$pattern/[^/]*$! ;
}

sub dtstring
{ my ($string,%xml)=@_;

  %ty=(%{$xml{'-type'}}, -ROOT => "NONE");

  # create a new XML::Parser instance using Tree Style
  if (defined($xml{-inputenc}) && ($xml{-inputenc} eq 'ISO-8859-1')){
     $parser = new XML::Parser(Style => 'Tree',
                               ErrorContext => 2 ,
                               ProtocolEncoding => 'ISO-8859-1');
  }
  else { $parser = new XML::Parser(Style => 'Tree',
                                   ErrorContext => 2 ,
                                  );
  }

  #execute Begin action if she exists
  if ($xml{-begin}){ &{$xml{-begin}} }

  # Convert XML to Perl code (Tree)
  $tree = $parser->parse($string);

  if($xml{-end}){ $c= omni("-ROOT",\%xml,@$tree); 
                  &{$xml{-end}} }
  else          { omni("-ROOT",\%xml,@$tree) }
}

sub omni{
  my ($par,$xml,@l) = @_;
  my $type = $ty{$par} || "STR";
  my %typeargs=();
  if(ref($type) eq "mmapon"){ 
       for(@$type){$typeargs{$_}=1;}
       $type = "MMAPON";
       };

  my $r ;

  if( $type eq 'STR')                                 { $r = "" }
  elsif( $type eq 'SEQ' or $type eq "ARRAY")          { $r = [] }
  elsif( $type eq 'SEQH' or $type eq "ARRAYOFHASH")   { $r = [] }
  elsif( $type eq 'MAP' or $type eq "HASH")           { $r = {} }
  elsif( $type eq 'MULTIMAP')                         { $r = {} }
  elsif( $type eq 'MMAPON' or $type eq "HASHOFARRAY") { $r = {} }
  elsif( $type eq 'NONE')                             { $r = "" }

  my ($name, $val, @val,$atr, $aux);
  while( @l) {
    ($name, $val, @l) = @l;
    if ($name eq "0"){ 
      $name="-pcdata";
      $aux= (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1')
              ?lat1::utf8($val): $val ;
      if(defined $xml->{-pcdata}) {
         push(@dtcontext,"-pcdata");
         $c=$aux; 
         $aux=&{$xml->{-pcdata}}; 
         pop(@dtcontext);
      }
    }
    else  {($atr,@val) = @$val;
           push(@dtcontext,$name);
           unshift(@dtatributes,$atr);
           $aux = omniele($xml, $name, omni($name,$xml,@val), $atr);
           shift(@dtatributes);
           pop(@dtcontext);
    }
    if   ($type eq "STR"){ $r .= $aux ;}
    elsif($type eq "SEQ" or $type eq "ARRAY"){
          push(@$r,$aux) unless whitepc($aux,$name);}
    elsif($type eq "SEQH" or $type eq "ARRAYHASH"){
          push(@$r,{"-c" => $aux,
                    "-q" => $name, 
                    %$atr }) unless whitepc($aux,$name);}
    elsif($type eq "MMAPON"){
          if(not whitepc($aux,$name)){
          if(! $typeargs{$name}) {
              warn "duplicated tag ´$name´\n" if(defined($r->{$name}));
              $r->{$name}=$aux }
          else { push(@{$r->{$name}},$aux) unless whitepc($aux,$name)}}
          }
    elsif($type eq "MAP" or $type eq "HASH"){
          if(not whitepc($aux,$name)){
          warn "duplicated tag ´$name´\n" if(defined($r->{$name}));
          $r->{$name}=$aux }}
    elsif($type eq "MULTIMAP"){
          push(@{$r->{$name}},$aux) unless whitepc($aux,$name)}
    elsif($type eq "NONE"){ $r=$aux;}
    else { $r="undefined type !!!"}
  }
  $r;
}

sub omniele {
  my $xml=shift;
  my $aux;
  ($q,$c,$aux)=@_; 
  %v=%$aux;
  if (defined($xml->{-outputenc}) && $xml->{-outputenc} eq 'ISO-8859-1'){
   for (keys %v){ $v{$_} = lat1::utf8($v{$_}) ; }
  }

  if   (defined $xml->{$q}) {&{$xml->{$q}} }
  elsif(defined $xml->{'-default'}) {&{$xml->{'-default'} }}
  else {toxml();}
}

sub toxmlp {
  my($q,$v,$c ) = @_;
  if($q eq "-pcdata") { $c}
  else {"<$q".  join("",map {" $_=\"$v->{$_}\""} keys %$v ) . ">$c</$q>" }
}

sub toxml {
     my %at=(seqitem => "item", @_);
     if(not ref($c)){  toxmlp($q,\%v,$c)}
     elsif (ref($c) eq "ARRAY") { 
       if($ty{$q} eq "SEQH") {
          toxmlp($q,{},
             join("",map {my %a=%$_; 
                          delete @a{"-q","-c"}; 
                          toxmlp($_->{-q},\%a,$_->{-c}) } @{$c} )) 
       }
       else { toxmlp($q,\%v,
           join("",map {"<$at{seqitem}>$_</$at{seqitem}>\n" } @{$c} ) )}
     }
     elsif (ref($c) eq "HASH") {   "<$q".
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

%mkdtskel=( '-default' => sub{$element{$q}=1; 
                              for (keys %v){$att{$q}{$_}=1 }; ""},

             '-end' => sub{ print <<'END';
#!/usr/bin/perl
use XML::DT ;
my $filename = shift;

%handler=(
#    '-outputenc' => 'ISO-8859-1',
#    '-default'   => sub{"<$q>$c</$q>"},
END

for $name (keys %element){
     print "     '$name' => sub{\"\$q:\$c\"},";
     print '# remember $v{',
            join('},$v{',keys %{$att{$name}}),
            '}' if $att{$name}; 
     print "\n";
}
print <<'END';
);
print dt($filename,%handler); 
END
            });

sub mkdtskel{ dt(shift,%mkdtskel)}

sub mkdtdskel {
  my @files = @_;
  my %handler=(
    '-outputenc' => 'ISO-8859-1',
    '-default'   => sub{ 
          $elel{$q}=1;
          $root = $q unless ctxt(1);
          $ele{ctxt(1)}{$q} = 1;
          for(keys(%v)){$att{$q}{$_} = 1 } ;
        },
    '-pcdata'    => sub{ if ($c =~ /[^ \t\n]/){ $ele{ctxt(1)}{"#PCDATA"}=1 }},
  );

  for $filename (@files){
  dt($filename,%handler); 
  }

  print "<!-- DTD $root ... -->\n<!-- (C) ... " . `date`." -->\n";
  delete $elel{$root};

  for ($root, keys %elel){
    putele($_);
    for $name (keys(%{$att{$_}})) {
       print( "\t<!-- $name : ... -->\n");
       print( "\t<!ATTLIST $_ $name CDATA #IMPLIED >\n");
    }
  }
}
  
sub putele {
  my $e = shift; 
  my @f ;
  if($ele{$e}){
     @f = keys %{$ele{$e}};
     print "<!ELEMENT $e (", join("|", @f ),")", (@f == 1 ? "": "*")," >\n";
  }
  else {
     print "<!ELEMENT $e  EMPTY >\n";}
}


sub whitepc{ $_[1] eq '-pcdata' and $_[0] =~ /^[ \t\n]*$/ };

sub MMAPON{ bless([@_],"mmapon") };
sub SEQOF{ bless([@_],"seqof") };

package lat1;

=head1 NAME

C<lat1.pm> - module for unicode utf8 to latin1 translation

=head1 SYNOPSIS

   $latin1string = lat1::utf8($utf8string)

=head1 Bugs

Translating the laint1 subset of unicode utf8 is very simples and needs no
tables.

If you need more complex translation, see the perl modules about unicode
and the C<recode> command.

=cut

sub utf8{
  my $t=shift;
  $t =~ s/([ÃÂ])(.)/ $1 eq "Ã" ? chr( ord($2) | 0100): $2 /ge;
  $t;
}

1;
