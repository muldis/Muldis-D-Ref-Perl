use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use Muldis::D::RefEng::Low_Level 0.000000;

###########################################################################
###########################################################################

{ package Muldis::D::RefEng; # package
    BEGIN {
        our $VERSION = '0.000000';
        $VERSION = eval $VERSION;
    }
} # package Muldis::D::RefEng

###########################################################################
###########################################################################

1; # Magic true value required at end of a reusable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

Muldis::D::RefEng -
Reference Implementation of Muldis D Over Perl 5

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2013, Muldis Data Systems, Inc.

=cut
