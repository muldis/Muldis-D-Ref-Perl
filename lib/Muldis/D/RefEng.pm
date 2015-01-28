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

=head1 VERSION

This document describes Muldis::D::RefEng version 0.0.0 for Perl 5.

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

Muldis::D::RefEng is the reference implementation of the Muldis D language,
authority B<http://muldis.com>, version number B<0.200>.  While other
Muldis D implementations also may exist, this one over Perl 5 is the
canonical one.

TODO: Rewrite this paragraph.
The separate all-documentation distribution L<Muldis::D> also exists; it
was intended to be the formal definition of the Muldis D language, but it
is currently years out of date; until it catches up, Muldis::D::RefEng also
serves double-duty as the formal language definition.

The Muldis D language itself has as a primary influence the work of Chris
Date (C.J. Date) and Hugh Darwen whose home website is
L<http://www.thethirdmanifesto.com/>.

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

These Perl 5 packages that are in the current distribution:
L<Muldis::D::RefEng::Low_Level>.

These other Perl 5 packages: L<Set::Relation>.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2015, Muldis Data Systems, Inc.

L<http://www.muldis.com/>

Muldis::D::RefEng is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License (LGPL) as
published by the Free Software Foundation (L<http://www.fsf.org/>); either
version 3 of the License, or (at your option) any later version.  You
should have received a copy of the LGPL as part of the Muldis::D::RefEng
distribution, in the files named "LICENSE/LGPL" and "LICENSE/GPL" (the
LGPLv3 is defined as the terms of the GPLv3 plus extra permissions); if
not, see L<http://www.gnu.org/licenses/>.

If it is not feasible for you to employ Muldis::D::RefEng subject to the
terms of the LGPL, then the copyright holder of Muldis::D::RefEng can
provide you a customized proprietary license, often at no cost, so that it
is still possible for you to employ Muldis::D::RefEng to meet your needs.

Any versions of Muldis::D::RefEng that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits.  Muldis::D::RefEng is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  However, for an
additional fee, the copyright holders of Muldis::D::RefEng can sell you a
warranty for it.

While it is by no means required, the copyright holder of Muldis::D::RefEng
would appreciate being informed any time you create a modified version of
Muldis::D::RefEng that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

I<This documentation is pending.>

=head1 FORUMS

I<This documentation is pending.>

=cut
