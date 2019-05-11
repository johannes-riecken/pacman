#!/usr/bin/env perl
use v5.10;
use warnings;

use Pacman qw(createPoly);
use Data::Dumper;

my @img;
open my $fin, '<', 'heart.pbm';
$_ = <$fin>;
die unless /^P1$/;
while (($_ = <$fin>) =~ /^#/) {}
/(\d+) (\d+)/;
my $w = $1;
my $h = $2;
my @data = split //, join '', <$fin>;
while (@data) {
  # push @img, [splice @data, 0, $w];
  push @img, [splice @data, 0, $w];
}

# my @img = (
#   [0,0,0,0,0,],
#   [0,0,1,0,0,],
#   [0,1,0,1,0,],
#   [0,0,1,0,0,],
#   [0,0,0,0,0,],
# );
# my $h = @img;
# my $w = @{$img[0]};

say "gonna dump";
# say Dumper Pacman::neighbors(1,1,1,1);
say Dumper Pacman::filterCorners([[0,0],[-1,1],[-1,2],[-1,3]]);
# say Dumper \Pacman::createPoly(\@img, $w, $h);
# say Pacman::toSVG([Pacman::createPoly(\@img, $w,$h)]);
say "dumped";
