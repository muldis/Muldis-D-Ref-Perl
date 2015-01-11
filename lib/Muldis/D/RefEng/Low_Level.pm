use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

###########################################################################
###########################################################################

use B;
use Math::BigInt try => 'GMP';

###########################################################################
###########################################################################

# A ::Value object represents a single Muldis D value of any data type.
# A ::Value object may compose other ::Value objects thus forming a value
# graph, but normally wouldn't compose things not conceptually values.
# You make a ::Value object using ::Low_Level methods.

{ package Muldis::D::RefEng::Low_Level::Value; # class
    BEGIN {
        our $VERSION = '0.000000';
        $VERSION = eval $VERSION;
    }
} # class Muldis::D::RefEng::Low_Level::Value

###########################################################################
###########################################################################

# The ::Low_Level class is implemented as a singleton partly since
# it might mediate various resources shared by multiple otherwise
# independent parts of a Perl application process.
# All Muldis::D::RefEng::Low_Level->select_MDLL() will get the same object.

{ package Muldis::D::RefEng::Low_Level; # class
    BEGIN {
        our $VERSION = '0.000000';
        $VERSION = eval $VERSION;
    }

    use Carp 'confess';
    use Scalar::Util 'blessed', 'refaddr';

    # Copy of _refcount_pp() from Devel::Refcount.
    # If we add that CPAN module as dependency, gives us 3X speed from XS.
    sub _refcount { B::svref_2object( shift @_ )->REFCNT; };

    # NAMING CONVENTIONS:
        # $h : value handle : a Perl refref blessed into the ::Value class
            # that refref points to a value struct
        # $s : value struct : a Perl hashref defining the value itself
        # $k : value kind : a Perl string given to ::Value.new() that says
            # what kind of low-level type we are selecting a value for
        # $p : value payload : something given to ::Value.new() that says
            # which value we are selecting, within the context of k.

    # Internal identifiers of each value struct attribute if it exists.
        my $VSA_S_KIND = 0;  # value struct kind, aka $k; determines other attrs
        # Valid $s{$MSA_S_KIND} aka $k values:
            my $S_KIND_BOOL  = 0;  # value of type Boolean
            my $S_KIND_INT   = 1;  # value of type Integer
            my $S_KIND_ARRAY = 2;  # value of type Array
            my $S_KIND_STR   = 3;  # value of type String
            my $S_KIND_DICT  = 4;  # value of type Dictionary
            my $S_KIND_TUPLE = 5;  # value of type Tuple
            my $S_KIND_CPSL  = 6;  # value of type Capsule
            my $S_KIND_IDENT = 7;  # value of type Identifier
        my $VSA_BOOL_AS_SV = 1;  # Perl native boolean if exists; (1==0) or (1==1)
        my $VSA_INT_AS_SV  = 1;  # Perl native integer if exists (IV and/or SV)
        my $VSA_INT_AS_BIG = 2;  # Math::BigInt object if exists
        my $VSA_ARRAY_AS_AV = 1;  # Perl arrayref of ::Value objects with $k in 0..7
        my $VSA_STR_AS_SV = 1;  # Perl native string if exists (either octet or character?)
        # my $VSA_STR_AS_AV = 2;  # Perl arrayref if exists of Perl native integer
        my $VSA_DICT_C_TODO = 1;  # TODO, list of components when type is a Dict, eg form of a B+tree or something
        my $VSA_TUPLE_AS_HV = 1;  # Perl hashref if exists where keys are Perl native strings and values are ::Value objects with $k in 0..7
        # my $VSA_TUPLE_AS_AV = 2;  # Perl arrayref if exists of arrayrefs; like the HV but keys are Perl arrayrefs of integers
        my $VSA_CPSL_C_TYPE  = 1;  # ::Value object of type Identifier
        my $VSA_CPSL_C_ATTRS = 2;  # ::Value object of type Tuple
        my $VSA_IDENT_C_PKG_NAME_BASE       = 1;  # ::Value object of type Array of type String
        my $VSA_IDENT_C_PKG_NAME_EXT        = 2;  # ::Value object of type Array of type String
        my $VSA_IDENT_C_REL_STARTS_N_LEV_UP = 3;  # ::Value object of type Integer (nonnegative)
        my $VSA_IDENT_C_PATH_BENEATH_PKG    = 4;  # ::Value object of type Array of type String
        my $VSA_IDENT_WHICH                 = 5;  # Perl native string if exists serializing the 4 components for use as hash key or fast use

    # ATTRIBUTE LIST OF ::LowLevel OBJECTS:
        # TODO

    my $MDLL = bless {}, __PACKAGE__;

    my $cache = {
        $S_KIND_BOOL  => {},
        $S_KIND_INT   => {},
        $S_KIND_ARRAY => {},
        $S_KIND_STR   => {},
        $S_KIND_DICT  => {},
        $S_KIND_TUPLE => {},
        $S_KIND_CPSL  => {},
        $S_KIND_IDENT => {},
    };

    # Boolean false and true values.
    my $false = $cache->{$S_KIND_BOOL}->{(1==0)} = _new_v( {
        $VSA_S_KIND     => $S_KIND_BOOL,
        $VSA_BOOL_AS_SV => (1==0),
    } );
    my $true = $cache->{$S_KIND_BOOL}->{(1==1)} = _new_v( {
        $VSA_S_KIND     => $S_KIND_BOOL,
        $VSA_BOOL_AS_SV => (1==1),
    } );

    # Integer 0 and 1 values.
    my $zero = $cache->{$S_KIND_INT}->{0} = _new_v( {
        $VSA_S_KIND    => $S_KIND_INT,
        $VSA_INT_AS_SV => 0,
    } );
    my $one = $cache->{$S_KIND_INT}->{1} = _new_v( {
        $VSA_S_KIND    => $S_KIND_INT,
        $VSA_INT_AS_SV => 1,
    } );

###########################################################################

sub select_MDLL
{
    return $MDLL;
}

sub _new_v
{
    my ($s) = @_;
    return bless \$s, __PACKAGE__.'::Value';
}

###########################################################################

sub v_Boolean
{
    my (undef, $p) = @_;
    # Expect $p to be a defined Perl native boolean.
    return $cache->{$S_KIND_BOOL}->{$p};
}

sub Boolean_as_SV
{
    my (undef, $h) = @_;
    # Expect $h to be an Boolean.
    return $$h->{$VSA_BOOL_AS_SV};
}

###########################################################################

sub v_Integer
{
    my (undef, $p) = @_;
    if (!ref $p)
    {
        # Expect $p to be a defined Perl native integer, IV or SV.
        if (exists $cache->{$S_KIND_INT}->{$p})
        {
            return $cache->{$S_KIND_INT}->{$p};
        }
        my $h = _new_v( {
            $VSA_S_KIND    => $S_KIND_INT,
            $VSA_INT_AS_SV => $p,
        } );
        if ($p >= -128 and $p <= 127)
        {
            $cache->{$S_KIND_INT}->{$p} = $h;
        }
        return $h;
    }
    else
    {
        # Expect $p to be a Math::BigInt object.
        if ($p->is_zero())
        {
            return $zero;
        }
        if ($p->is_one())
        {
            return $one;
        }
        return _new_v( {
            $VSA_S_KIND     => $S_KIND_INT,
            $VSA_INT_AS_BIG => $p,
        } );
    }
}

sub Integer_as_SV
{
    my (undef, $h) = @_;
    # Expect $h to be an Integer.
    if (!exists $$h->{$VSA_INT_AS_SV})
    {
        $$h->{$VSA_INT_AS_SV} = $$h->{$VSA_INT_AS_BIG}->bstr();
    }
    return $$h->{$VSA_INT_AS_SV};
}

sub Integer_as_BigInt
{
    my (undef, $h) = @_;
    # Expect $h to be an Integer.
    if (!exists $$h->{$VSA_INT_AS_BIG})
    {
        $$h->{$VSA_INT_AS_BIG} = Math::BigInt->new( $$h->{$VSA_INT_AS_SV} );
    }
    return $$h->{$VSA_INT_AS_BIG};
}

###########################################################################

sub MDLL__Universal__same
{
    my (undef, $h_lhs, $h_rhs) = @_;
    if (refaddr $h_lhs == refaddr $h_rhs)
    {
        # Caller used the same ::Value object/handle in multiple places.
        return $true;
    }
    if (refaddr $$h_lhs == refaddr $$h_rhs)
    {
        # Caller created 2 ::Value objects/handles likely independently but
        # then probably called same() on them so their structs were merged.
        return $true;
    }
    if ($$h_lhs->{$VSA_S_KIND} != $$h_rhs->{$VSA_S_KIND})
    {
        return $false;
    }
    my $result_p;
    my $k = $$h_lhs->{$VSA_S_KIND};
    if ($k == $S_KIND_BOOL)
    {
        confess q{we should never get here due to prior refaddr tests};
    }
    elsif ($k == $S_KIND_INT)
    {
        $result_p = ($MDLL->Integer_as_SV($h_lhs)
            eq $MDLL->Integer_as_SV($h_rhs));
    }
    else
    {
        confess q{not implemented};
    }
    if ($result_p)
    {
        # Caller created 2 ::Value objects/handles likely independently but
        # representing the same Muldis D value; we will merge their value
        # structs to save memory and speed up future same() calls.
        # We will merge to the one that already has more refs to it.
        if (_refcount( $$h_lhs ) < _refcount( $$h_rhs ))
        {
            $$h_lhs = $$h_rhs;
        }
        else
        {
            $$h_rhs = $$h_lhs;
        }
    }
    return $result_p ? $true : $false;
}

###########################################################################

sub MDLL__Boolean__false
{
    return $false;
}

sub MDLL__Boolean__true
{
    return $true;
}

sub MDLL__Boolean__not
{
    my (undef, $h) = @_;
    return (refaddr $h == refaddr $true) ? $true : $false;
}

sub MDLL__Boolean__and
{
    my (undef, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true) ? $h_rhs : $false;
}

sub MDLL__Boolean__or
{
    my (undef, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true) ? $true : $h_rhs;
}

sub MDLL__Boolean__xor
{
    my (undef, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true)
        ? $MDLL->MDLL__Boolean__not($h_rhs) : $h_rhs;
}

###########################################################################

sub MDLL__Integer__zero
{
    return $zero;
}

sub MDLL__Integer__is_zero
{
    my (undef, $h) = @_;
    return (refaddr $h == refaddr $zero) ? $true : $false;
}

sub MDLL__Integer__one
{
    return $one;
}

sub MDLL__Integer__is_one
{
    my (undef, $h) = @_;
    return (refaddr $h == refaddr $one) ? $true : $false;
}

sub MDLL__Integer__plus
{
    my (undef, $h_augend, $h_addend) = @_;
    if (refaddr $h_augend == refaddr $zero)
    {
        return $h_addend;
    }
    if (refaddr $h_addend == refaddr $zero)
    {
        return $h_augend;
    }
    if (exists $$h_augend->{$VSA_INT_AS_BIG}
        or exists $$h_addend->{$VSA_INT_AS_BIG})
    {
        # At least one input is a Math::BigInt object.
        return $MDLL->v_Integer( Math::BigInt->badd(
            exists $$h_augend->{$VSA_INT_AS_BIG}
                ? $$h_augend->{$VSA_INT_AS_BIG}
                : $$h_augend->{$VSA_INT_AS_SV},
            exists $$h_addend->{$VSA_INT_AS_BIG}
                ? $$h_addend->{$VSA_INT_AS_BIG}
                : $$h_addend->{$VSA_INT_AS_SV},
        ) );
    }
    # Both inputs are native Perl integers.
    my $sum = $$h_augend->{$VSA_INT_AS_SV} + $$h_addend->{$VSA_INT_AS_SV};
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $sum ne $sum)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $sum = Math::BigInt->badd( $$h_augend->{$VSA_INT_AS_SV},
            $$h_addend->{$VSA_INT_AS_SV} 
        );
    }
    return $MDLL->v_Integer( $sum );
}

###########################################################################

} # class Muldis::D::RefEng::Low_Level

###########################################################################
###########################################################################

1; # Magic true value required at end of a reusable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

Muldis::D::RefEng::Low_Level -
Native Perl 5 implementation of Muldis_D::Low_Level package

=head1 SYNTAX

    use Muldis::D::RefEng::Low_Level;

    my $MDLL = Muldis::D::RefEng::Low_Level->select_MDLL();

    my $a = $MDLL->v_Integer(42);
    my $b = $MDLL->v_String([432,23,42,55]);
    my $c = $MDLL->v_String('hello');

=head1 DESCRIPTION

This file is used internally by L<Muldis::D::RefEng>; it is not intended to
be used directly in user code.

=head1 INTERFACE

...

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1.

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2013, Muldis Data Systems, Inc.

=cut
