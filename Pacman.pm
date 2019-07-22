#!/usr/bin/env perl
package Pacman;
use v5.10;
use warnings;

# use List::AllUtils qw(:all);
# use List::Util;
# use List::MoreUtils;
# use Data::Dumper;
# use Math::Complex;
# use POSIX qw(round);

# neighbors :: Natural -> Natural -> Int -> Int -> [(Int,Int)]
# @x: x-coordinate of reference point
# @y: y-coordinate of reference point
# @dir_x: (optional) x coordinate of starting direction vector (0 or +/-1)
# @dir_y: (optional) y coordinate of starting direction vector (0 or +/-1)
# Calculates all eight neighbors of a point, some of which may be negative.
# Returns: (array fixed-size=8) List of the coordinates of all neighbors of the reference point.
sub neighbors {
    my ($x, $y, $dir_x, $dir_y) = @_;
    $dir_x //= 1;
    $dir_y //= 0;
    my $z = cplx(1/sqrt(2),1/sqrt(2));
    my @ret;
    my $vect = cplx($dir_x, $dir_y);
    for (my ($i, $vect) = (0, cplx($dir_x, $dir_y)); $i < 8; $i++, $vect *= $z) {  # eight surrounding points
        push @ret, [$x + round(Re($vect)), $y + round(Im($vect))];
    }
    return @ret;
}

# filterInBounds :: [(Int,Int)] -> Natural -> Natural -> [(Natural,Natural)]
# @pnts: list of points, some of which may be negative
# @w: width of image
# @h: height of image
sub filterInBounds {
  my ($pnts, $w, $h);
  my @res;
  return
    grep { $$_[0] >= 0 && $$_[0] < $w && $$_[1] >= 0 && $$_[1] < $h } @$pnts;
}

# firstWhiteNeighbor :: [(Natural,Natural)] -> [(Natural,Natural)] -> Natural -> Natural -> Int -> Int
# @img: jagged array of pixels with color info as integer
# @seen: list of seen points to exclude
# @x: x-coordinate of reference point
# @y: y-coordinate of reference point
# @dir_x: x-coordinate of starting direction vector
# @dir_y: y-coordinate of starting direction vector
sub firstWhiteNeighbor {
  my ($img, $seen, $x, $y, $dir_x, $dir_y) = @_;
  # calculate all neighbors
  my $neighbors = \neighbors($x, $y, $dir_x, $dir_y);
  # exclude points out of boinds
  $neighbors = \filterInBounds($neighbors, 0+@$img[0], 0+@$img);
  # exclude points in @seen
  $neighbors = \setDifference($neighbors, $seen);
  # return first white point
  return first { $$img[$$_[1]][$$_[0]] > 0 } @$neighbors;
}

sub setDifference {
  my ($a, $b) = @_;
  my %aOnly;
  @aOnly{@$a} = ();
  delete @aOnly{@$b};
  return keys %aOnly;
}

# walkLine :: [[Int]] -> Natural -> Natural -> Set [(Natural,Natural)] -> ([(Natural,Natural)],Set [(Natural,Natural)])
# @img: jagged array of pixels with color info as integer
# @x: x-coordinate of starting pixel
# @y: y-coordinate of starting pixel
# walk along line in image starting at ($x,$y) and return all points on line. Line is white (nonzero)
# Return: All points on line starting at point
sub walkLine {
    my ($img, $x, $y) = @_;
    my ($xx, $yy) = ($x, $y);
    my ($x_prev, $y_prev);
    my @ret = [$xx, $yy];
    for (my ($xx, $yy) = ($x, $y);
         defined $xx && defined $yy;
         ($x_prev, $y_prev) = ($xx, $yy), ($xx, $yy) = firstWhiteNeighbor($img, \@ret, $xx, $yy)) {
        push @ret, [$xx, $yy];    # push new found point on line
    }
    return @ret;
}

# pointEq
# @p0: First point to compare by equality
# @p1: Second point to compare by equality
# Returns: 1 if equal, undef if not
sub pointEq {
  my ($p0, $p1) = @_;
  return $$p0[0] == $$p1[0] && $$p0[1] == $$p1[1];
}

# pointDiff
# @p0: Point to subtract from
# @p0: Point to subtract
# Returns: Result of subtraction, vector from p1 to p0
sub pointDiff {
  my ($p0, $p1) = @_;
  my @res;
  $res[0] = $$p0[0] - $$p1[0];
  $res[1] = $$p0[1] - $$p1[1];
  return @res;
}

# filterCorners :: [(Int,Int)] -> [(Int,Int)]
# @pnts = jagged array of coordinates of white points
# TODO: Maybe use grep
sub filterCorners {
    my @pnts = @{$_[0]};
    for (0..1) {
        my $pnt = shift @pnts;
        if ($pnt) { # if shifting did return something
            push @res, $pnt;
        }
    }
    # add points to @res and remove them again if they are not corner points
    return \@res unless @pnts;
    my @dir0 = pointDiff($res[1], $res[0]);
    while (my $pnt = shift @pnts) {
        my @dir1 = pointDiff($pnt, $res[-1]);
        if (pointEq(\@dir0, \@dir1)) {
            pop @res;
        }
        push @res, $pnt;
        @dir0 = @dir1;
    }
    return \@res;
}

# createPoly :: [[Int]] -> [(Int,Int)]
# @img = jagged array of image coordinate values
# Returns: list of polygon points
sub createPoly {
    my ($img) = @_;
    my @img = @$img;
    my $h = @img;
    my $w = @{$img[0]};
    my %seen;
    my @polylines;
    for my $y (0..$h - 1) {
        my $y_orig = $y;
        for my $x (0..$w - 1) {
            if($img[$y][$x] && !$seen{$x,$y}) {
                $seen{$x,$y} = 1;
                push @polylines, [];
                push @{$polylines[-1]}, [$x,$y];
                my @dir;
                my ($xx,$yy);
                my $is_seen;
                WALKLINE: while (1) {
                    for my $pnt (filterInBounds(\neighbors($x,$y,@dir))) {
                        ($xx,$yy) = @$pnt;
                        $is_seen = $seen{$xx,$yy};
                        if ($img[$yy][$xx]) {
                            my @dir2 = ($xx - $x, $yy - $y);
                            if (@dir && @dir2 && ($dir2[0] != $dir[0] || $dir2[1] != $dir[1])) {
                                push @{$polylines[-1]}, [$x,$y];
                            }
                            $seen{$xx,$yy} = 1;
                            ($x,$y) = ($xx,$yy);
                            @dir = @dir2;
                            if ($is_seen) {
                                last WALKLINE;
                            }
                            next WALKLINE;
                        }
                    }
                    last;
                }
                if ($is_seen) {
                    push @{$polylines[-1]}, [-1,-1];
                } else {
                    push @{$polylines[-1]}, [$x,$y];
                }
                $y = $y_orig;
            }
        }
    }
    return @polylines;
}

sub toSVG {
    my @polylines = @{$_[0]};
    my $ret = '<svg width="200" height="250" version="1.1" xmlns="http://www.w3.org/2000/svg">';
    for (@polylines) {
        $ret .= '<polyline stroke="black" fill="none" points="';
        for (@$_) {
            my @p = @{$_};
            $ret .= $p[0] == -1 ? 'z' : "$p[0] $p[1] ";
        }
        $ret .= '"/>';
    }
    $ret .= '</svg>';
    return $ret;
}

sub round {
  my $float = $_[0];
  int($float + $float/abs($float*2 || 1));
}

sub cplx {
        return __PACKAGE__->make(@_);
}

sub Re {
        my ($z, $Re) = @_;
        return $z unless ref $z;
        if (defined $Re) {
            $z->{'cartesian'} = [ $Re, ${$z->_cartesian}[1] ];
            $z->{c_dirty} = 0;
            $z->{p_dirty} = 1;
        } else {
            return ${$z->_cartesian}[0];
        }
}

sub Im {
        my ($z, $Im) = @_;
        return 0 unless ref $z;
        if (defined $Im) {
            $z->{'cartesian'} = [ ${$z->_cartesian}[0], $Im ];
            $z->{c_dirty} = 0;
            $z->{p_dirty} = 1;
        } else {
            return ${$z->_cartesian}[1];
        }
}

use overload
        '='     => \&_copy,
        '+='    => \&_plus,
        '+'     => \&_plus,
        '-='    => \&_minus,
        '-'     => \&_minus,
        '*='    => \&_multiply,
        '*'     => \&_multiply,
        '/='    => \&_divide,
        '/'     => \&_divide,
        '**='   => \&_power,
        '**'    => \&_power,
        '=='    => \&_numeq,
        '<=>'     => \&_spaceship,
        'neg'   => \&_negate,
        '~'     => \&_conjugate,
        'abs'   => \&abs,
        #        'sqrt'  => \&sqrt,
        'exp'   => \&exp,
        'log'   => \&log,
        'sin'   => \&sin,
        'cos'   => \&cos,
        'atan2' => \&atan2,
        '""'    => \&_stringify;
        #

sub _set_cartesian { $_[0]->{p_dirty}++; $_[0]->{c_dirty} = 0;
                     $_[0]->{'cartesian'} = $_[1] }
                   
my $gre = qr'\s*([\+\-]?(?:(?:(?:\d+(?:_\d+)*(?:\.\d*(?:_\d+)*)?|\.\d+(?:_\d+)*)(?:[eE][\+\-]?\d+(?:_\d+)*)?))|inf)'i;

sub display_format {
        my $self  = shift;
        my %display_format = %DISPLAY_FORMAT;
 
        if (ref $self) {                        # Called as an object method
            if (exists $self->{display_format}) {
                my %obj = %{$self->{display_format}};
                @display_format{keys %obj} = values %obj;
            }
        }
        if (@_ == 1) {
            $display_format{style} = shift;
        } else {
            my %new = @_;
            @display_format{keys %new} = values %new;
        }
 
        if (ref $self) { # Called as an object method
            $self->{display_format} = { %display_format };
            return
                wantarray ?
                    %{$self->{display_format}} :
                    $self->{display_format}->{style};
        }
 
        # Called as a class method
        %DISPLAY_FORMAT = %display_format;
        return
            wantarray ?
                %DISPLAY_FORMAT :
                    $DISPLAY_FORMAT{style};
}
sub make {
    my $self = bless {}, shift;
    my ($re, $im);
    if (@_ == 0) {
        ($re, $im) = (0, 0);
    } elsif (@_ == 2) {
        ($re, $im) = @_;
    }
    if (defined $re) {
        _cannot_make("real part",      $re) unless $re =~ /^$gre$/;
    }
    $im ||= 0;
    _cannot_make("imaginary part", $im) unless $im =~ /^$gre$/;
    $self->_set_cartesian([$re, $im ]);
    $self->display_format('cartesian');
 
    return $self;
}

sub _cartesian {$_[0]->{c_dirty} ?
                   $_[0]->_update_cartesian : $_[0]->{'cartesian'}}

sub _emake {
    my $arg = shift;
    my ($p, $q);
 
    if ($arg =~ /^\s*\[\s*$gre\s*(?:,\s*$gre\s*)?\]\s*$/) {
        ($p, $q) = ($1, $2 || 0);
    } elsif ($arg =~ m!^\s*\[\s*$gre\s*(?:,\s*([-+]?\d*\s*)?pi(?:/\s*(\d+))?\s*)?\]\s*$!) {
        ($p, $q) = ($1, ($2 eq '-' ? -1 : ($2 || 1)) * pi() / ($3 || 1));
    } elsif ($arg =~ /^\s*\[\s*$gre\s*\]\s*$/) {
        ($p, $q) = ($1, 0);
    } elsif ($arg =~ /^\s*$gre\s*$/) {
        ($p, $q) = ($1, 0);
    }
 
    if (defined $p) {
        $p =~ s/^\+//;
        $q =~ s/^\+//;
        $p =~ s/^(-?)inf$/"${1}9**9**9"/e;
        $q =~ s/^(-?)inf$/"${1}9**9**9"/e;
    }
 
    return ($p, $q);
}
 
#
# (_plus)
#
# Computes z1+z2.
#
sub _plus {
        my ($z1, $z2, $regular) = @_;
        my ($re1, $im1) = @{$z1->_cartesian};
        $z2 = cplx($z2) unless ref $z2;
        my ($re2, $im2) = ref $z2 ? @{$z2->_cartesian} : ($z2, 0);
        unless (defined $regular) {
                $z1->_set_cartesian([$re1 + $re2, $im1 + $im2]);
                return $z1;
        }
        return (ref $z1)->make($re1 + $re2, $im1 + $im2);
}
 
#
# (_minus)
#
# Computes z1-z2.
#
sub _minus {
        my ($z1, $z2, $inverted) = @_;
        my ($re1, $im1) = @{$z1->_cartesian};
        $z2 = cplx($z2) unless ref $z2;
        my ($re2, $im2) = @{$z2->_cartesian};
        unless (defined $inverted) {
                $z1->_set_cartesian([$re1 - $re2, $im1 - $im2]);
                return $z1;
        }
        return $inverted ?
                (ref $z1)->make($re2 - $re1, $im2 - $im1) :
                (ref $z1)->make($re1 - $re2, $im1 - $im2);
 
}
 
#
# (_multiply)
#
# Computes z1*z2.
#
sub _multiply {
        my ($z1, $z2, $regular) = @_;
        if ($z1->{p_dirty} == 0 and ref $z2 and $z2->{p_dirty} == 0) {
            # if both polar better use polar to avoid rounding errors
            my ($r1, $t1) = @{$z1->_polar};
            my ($r2, $t2) = @{$z2->_polar};
            my $t = $t1 + $t2;
            if    ($t >   pi()) { $t -= pi2 }
            elsif ($t <= -pi()) { $t += pi2 }
            unless (defined $regular) {
                $z1->_set_polar([$r1 * $r2, $t]);
                return $z1;
            }
            return (ref $z1)->emake($r1 * $r2, $t);
        } else {
            my ($x1, $y1) = @{$z1->_cartesian};
            if (ref $z2) {
                my ($x2, $y2) = @{$z2->_cartesian};
                return (ref $z1)->make($x1*$x2-$y1*$y2, $x1*$y2+$y1*$x2);
            } else {
                return (ref $z1)->make($x1*$z2, $y1*$z2);
            }
        }
}


sub _update_cartesian {
        my $self = shift;
        my ($r, $t) = @{$self->{'polar'}};
        $self->{c_dirty} = 0;
        return $self->{'cartesian'} = [$r * CORE::cos($t), $r * CORE::sin($t)];
}

1;
