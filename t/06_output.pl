#!/usr/bin/perl
use XML::DT ;
my $filename = shift;

%handler=(
#    '-outputenc' => 'ISO-8859-1',
#    '-default'   => sub{"<$q>$c</$q>"},
     'a' => sub{
       # occurred 1 times
       "$q:$c"
     },
     'b' => sub{
       # remember attributes $v{title}
       # occurred 3 times
       "$q:$c"
     },
     'c' => sub{
       # remember attributes $v{title}
       # occurred 6 times
       "$q:$c"
     },
);
print dt($filename,%handler);
