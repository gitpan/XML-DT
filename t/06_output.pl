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
     'a' => sub{ }, # 1 occurrences;
     'b' => sub{ }, # 3 occurrences; attributes: title
     'c' => sub{ }, # 6 occurrences; attributes: title
);
print dt($filename,%handler);
