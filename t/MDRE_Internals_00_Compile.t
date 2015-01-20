use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use Test::More 0.47;

use_ok( 'Muldis::D::RefEng' );
is( $Muldis::D::RefEng::VERSION, 0.000000,
    'Muldis::D::RefEng is the correct version' );

done_testing();

1; # Magic true value required at end of a reusable file's code.
