#!/usr/bin/perl
use XML::DT ;
my $filename = shift;

# Variable Reference
#
# $c - contents after child processing
# $q - element name (tag)
# %v - hash of attributes

%handler=(
#    '-outputenc' => 'ISO-8859-1',
#    '-default'   => sub{"<$q>$c</$q>"},
     'a' => sub { },
     'b' => sub { }, # attributes: title
     'c' => sub { }, # attributes: title
);

print dt($filename,%handler);
