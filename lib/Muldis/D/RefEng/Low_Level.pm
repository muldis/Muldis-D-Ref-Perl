use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

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

# There is no class in ::Low_Level to represent a Muldis D variable partly
# given their non-trivial possibly-pseudo nature and so implementing such
# is left to other parts of RefEng.  Instead, any subject-to-update
# parameters take a Perl refref pointing to a ::Value object.

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
        # $MDLL : singleton obj repr Muldis_D::Low_Level package
        # $h : value handle : a Perl refref blessed into the ::Value class,
            # that refref points to a value struct
        # $s : value struct : a Perl hashref defining the value itself
        # $k : value kind : first attr of value struct, says the base type
        # $p : value payload : other main attr(s) of value struct, says
            # which value we are selecting, within the context of $k
        # $var : variable : unblessed Perl refref points to value handle/$h
        # select_MDLL() : submethod to obtain $MDLL object
        # v_[A-Z][a-z]+() : new/same ::Value obj def in terms of Perl values
        # v_[A-Z][a-z]+_as_[A-Za-z]+() : extract Perl payload from ::Value
        # [A-Z][a-z]+__[A-Za-z0-9_]+() : Muldis D rtns, take+return ::Value

    # Internal identifiers of each value struct attribute if it exists.
        my $VSA_S_KIND = 'k';  # value struct kind, aka $k; determines further attrs
            # Valid $s{$MSA_S_KIND} aka $k values:
            my $S_KIND_BOOL  = 'b';  # value of type Boolean
            my $S_KIND_INT   = 'i';  # value of type Integer
            my $S_KIND_ARRAY = 'a';  # value of type Array
            my $S_KIND_STR   = 's';  # value of type String
            my $S_KIND_DICT  = 'd';  # value of type Dictionary
            my $S_KIND_TUPLE = 't';  # value of type Tuple
            my $S_KIND_CPSL  = 'c';  # value of type Capsule
            my $S_KIND_IDENT = 'e';  # value of type Identifier
        my $VSA_WHICH = 'which';  # Perl native string/SV if exists,
            # or Perl arrayref of said string or arrayref,
            # for use when indexing this value by its default method,
            # such as for use as a Dict key particularly if a B+tree/etc;
            # for hashref key flatten any array in order appropriately;
            # is canonical format identity between all values of same $k;
            # (externally) prepend $k to arrayref for identity across all ::Value;
            # some type-specific attrs may alias to this if appropriate;
            # just flatten Identifier as S::R did, compon as-is with leading len counts,no escapes
        my $VSA_P_AS_SV = 'sv';  # Perl defined nonref scalar if exists
            # Further restriction/detail based on struct kind:
                # Boolean - Perl native boolean if exists; (1==0) or (1==1)
                # Integer - Perl native integer if exists (IV and/or SV)
                # String - Perl native string if exists, either octet or char,
                    # while API to take Muldis D String as Array of Integer exists,
                    # internally always stored as native Perl string,
                    # hence String elements outside Perl's said range not supported
        my $VSA_P_AS_BIGINT = 'bigint';  # Math::BigInt object if exists
        my $VSA_P_AS_AV = 'av';  # Array - Perl arrayref if exists of ::Value objects
        my $VSA_P_AS_HV = 'hv';  # Tuple - Perl hashref if exists of ::Value objects,
            # note about String limitations affects Tuple attr names also
        my $VSA_DICT_C_ELEMS = 'elems';  # the dictionary elements if exists
            # A tree structure conceptually holding set of key+value pairs
            # where the keys all mutually unique and values may not be.
            # Each key+value (DK+DV) typically a pair (DP) of ::Value, lives at a leaf.
            # This structure is typically O(1) for testing presence/absence
            # of a single DK when looked up using the whole of the DK value
            # rather than a portion/hash of it, and fetching its DV;
            # likewise, O(M) for looking up M DK values.
            # It is also typically O(N) to build the structure in the first
            # place from an array of N DP; likewise performance should be
            # not worse than that O(N+M) when deriving new Dict values from
            # existing ones in the form of DP inserts, deletes, unions,
            # intersects, diffs, and so on as we would try to share tree
            # sub-structures between the old and new Dicts.
            # See 'keys'+'indexes' for other ways of working with the DPs.
            # The creation of 'elems' is lazy iff any 'keys' exist and it
            # may never be created at all if not needed.
            # Tree root isa Perl hashref:
                # One elem per struct kind / low level type.
                    # Each DP is sorted into a bucket for its DK's $k.
                    # hkey is any value valid for $k.
                    # When DK's $k in {Boolean, Integer, String, Tuple,
                        # Capsule, Identifier}, hval is a Perl hashref;
                        # when $k in {Array, Dict}, hval is 2-elem arrayref.
                    # hval is hashref, k+v meaning/struct dep on DK's $k:
                        # Each {Boolean,Integer,String}-typed DK:
                            # hkey is just the DK's SV payload.
                            # hval is the DP.
                        # Each Tuple-typed DK:
                            # hkey is Tuple-specific serialization of the
                                # whole Tuple heading; its attr names are
                                # catenated in sorted order, w strlen meta.
                            # hval is a 2-elem Perl arrayref:
                                # We are storing all DP sharing a Tuple
                                # heading in a lazily built structure for
                                # performance/space reasons, especially as
                                # we may have a whole Relation here or
                                # otherwise a set of Tuple whose lookup
                                # strategy is better determined by a user
                                # provided explicit 'keys'.
                                # [0] is a Perl arrayref (eager):
                                    # Each elem is a DP, in arbitrary order
                                    # that may match the input order.
                                    # This array might have duplicates.
                                # [1] is a Perl hashref (lazy):
                                    # Each hval is a DP, whose corresp
                                    # hkey serializes whole Tuple body;
                                    # attr vals catenated in order corresp
                                    # to sorted attr names, w strlen meta.
                                    # We only populate this hashref when we
                                    # want to count the number of distinct
                                    # DP we have and no 'keys' exist.
                        # Each Capsule-typed DK:
                            # hkey is derived from DK "type" attr:
                                # Same as if DK were Identifer-typed.
                            # hval is derived from DK "attrs" attr:
                                # hval is a Perl hashref.
                                # Same format as elems[Tuple].
                        # Each Identifier-typed DK:
                            # hkey is Ident-specific serialization of DK.
                                # Its 4 attrs catenated in their defined
                                # order, recursively, with strlen meta/etc.
                            # hval is the DP.
                    # hval is 2-elem arrayref, elem meaning/struct dep on DK's $k:
                        # Each Array-typed DK:
                            # We are storing all Array in a lazily built
                            # structure like with Tuples but not split by
                            # heading, same reasons, same 'keys' effects.
                            # [0] is a Perl arrayref (eager):
                                # Each elem a DP, arbit order, possib dups.
                            # [1] is a Perl hashref (lazy):
                                # Each hval a DP; hkey serializes whole
                                # Array in index order.
                        # Each Dictionary-typed DK:
                            # We are storing all Dictionary in a lazily
                            # built structure like with Arrays.
                            # [0] is a Perl arrayref (eager):
                                # Each elem a DP, arbit order, possib dups.
                            # [1] is a Perl hashref (lazy):
                                # Each hval a DP; hkey serializes whole
                                # Dictionary by first serializing all
                                # elements in it and then catenating those
                                # strings in mutually sorted order.
        my $VSA_DICT_C_KEYS = 'keys';  # DPs indexed by unique idents
            # TODO, write this.
            # Using 'keys' is recommended as users (of Low_Level) can often
            # provide better indexing strategies than naive ones we use by
            # default for simplicity.  Omitting any 'keys' is fine/best
            # when our Dictionary just contains Low_Level scalar values but
            # it should be employed for {Tuple,Array,Dict,Capsule} elems.
            # Note that when using 'keys' Low_Level will just trust the
            # user that any values providing for indexing are unique between
            # all DKs used in the same Dict; if not, bugs may result;
            # in contrast, 'indexes' doesn't make that assumption.
        my $VSA_DICT_C_INDEXES = 'indexes';  # DPs ind by nonunique idents
            # TODO, rewrite this ...
            # users of Dict typically provide each elem as a triple of
            # {index str/aref, dict key ::Value, dict value ::Value}
            # within the context of naming an index to use;
            # if they just give {dkey,dvalue} then key's WHICH as well as
            # Dict's internal identity index is used instead;
            # they could also indicate multiple named indexes per value,
            # particularly if the Dict is implementing a Relation;
            # or more indexes could be added ad-hoc to a Dict ::Value later;
            # ? see also how Set::Relation works ...
            # FOR NOW lets make initial/default index/tree work like
            # $_cache meaning root has a child per low level type, then
            # children varying structure on said type; for SV-based types
            # its just the value at that point, for others its more tree
            # eg with tuple indexed by attrname;
            # Array could be ind by first several elems vals maybe,
            # Tuple by first several elems keys+vals maybe;
            # Capsule certainly by 'type' (as SV) first then by Tuple way;
            # we can change $_cache itself to be just a Dict I suppose
        my $VSA_CPSL_C_TYPE  = 1;  # ::Value object of type Identifier
        my $VSA_CPSL_C_ATTRS = 2;  # ::Value object of type Tuple
        my $VSA_IDENT_C_PKG_NAME_BASE       = 1;  # ::Value object of type Array of type String
        my $VSA_IDENT_C_PKG_NAME_EXT        = 2;  # ::Value object of type Array of type String
        my $VSA_IDENT_C_REL_STARTS_N_LEV_UP = 3;  # ::Value object of type Integer (nonnegative)
        my $VSA_IDENT_C_PATH_BENEATH_PKG    = 4;  # ::Value object of type Array of type String

    # ATTRIBUTE LIST OF ::LowLevel OBJECTS:
        # TODO

    my $_MDLL = bless {}, __PACKAGE__;

    my $_cache = {
        $S_KIND_BOOL  => {},
        $S_KIND_INT   => {},
        $S_KIND_STR   => {},
        $S_KIND_IDENT => {},
    };

    # Boolean false and true values.
    my $false = $_cache->{$S_KIND_BOOL}->{(1==0)} = _new_v( {
        $VSA_S_KIND  => $S_KIND_BOOL,
        $VSA_P_AS_SV => (1==0),
    } );
    my $true = $_cache->{$S_KIND_BOOL}->{(1==1)} = _new_v( {
        $VSA_S_KIND  => $S_KIND_BOOL,
        $VSA_P_AS_SV => (1==1),
    } );

    # Integer 0 and 1 values.
    my $zero = $_cache->{$S_KIND_INT}->{0} = _new_v( {
        $VSA_S_KIND  => $S_KIND_INT,
        $VSA_P_AS_SV => 0,
    } );
    my $one = $_cache->{$S_KIND_INT}->{1} = _new_v( {
        $VSA_S_KIND  => $S_KIND_INT,
        $VSA_P_AS_SV => 1,
    } );

    # Array with no elements.
    my $empty_array = _new_v( {
        $VSA_S_KIND  => $S_KIND_ARRAY,
        $VSA_P_AS_AV => [],
    } );

    # String with no elements.
    my $empty_str = $_cache->{$S_KIND_STR}->{0} = _new_v( {
        $VSA_S_KIND  => $S_KIND_STR,
        $VSA_P_AS_SV => q{},
    } );

    # Tuple with no elements.
    my $nullary_tuple = _new_v( {
        $VSA_S_KIND  => $S_KIND_TUPLE,
        $VSA_P_AS_HV => {},
    } );

###########################################################################

sub select_MDLL
{
    return $_MDLL;
}

sub _new_v
{
    my ($s) = @_;
    return bless \$s, __PACKAGE__.'::Value';
}

###########################################################################

sub v_Boolean
{
    my ($MDLL, $p) = @_;
    # Expect $p to be a defined Perl native boolean.
    return $_cache->{$S_KIND_BOOL}->{$p};
}

sub v_Boolean_as_SV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Boolean.
    return $$h->{$VSA_P_AS_SV};
}

###########################################################################

sub v_Integer
{
    my ($MDLL, $p, $want_cached) = @_;
    if (!ref $p)
    {
        # Expect $p to be a defined Perl native integer, IV or SV.
        if (exists $_cache->{$S_KIND_INT}->{$p})
        {
            return $_cache->{$S_KIND_INT}->{$p};
        }
        my $h = _new_v( {
            $VSA_S_KIND  => $S_KIND_INT,
            $VSA_P_AS_SV => $p,
        } );
        if ($want_cached or $p >= -128 and $p <= 127)
        {
            $_cache->{$S_KIND_INT}->{$p} = $h;
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
        my $h = _new_v( {
            $VSA_S_KIND      => $S_KIND_INT,
            $VSA_P_AS_BIGINT => $p,
        } );
        if ($want_cached)
        {
            $_cache->{$S_KIND_INT}->{$MDLL->v_Integer_as_SV($h)} = $h;
        }
        return $h;
    }
}

sub v_Integer_as_SV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Integer.
    if (!exists $$h->{$VSA_P_AS_SV})
    {
        $$h->{$VSA_P_AS_SV} = $$h->{$VSA_P_AS_BIGINT}->bstr();
    }
    return $$h->{$VSA_P_AS_SV};
}

sub v_Integer_as_BigInt
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Integer.
    if (!exists $$h->{$VSA_P_AS_BIGINT})
    {
        $$h->{$VSA_P_AS_BIGINT} = Math::BigInt->new( $$h->{$VSA_P_AS_SV} );
    }
    return $$h->{$VSA_P_AS_BIGINT};
}

###########################################################################

sub v_Array
{
    my ($MDLL, $p) = @_;
    # Expect $p to be a Perl arrayref.
    if (@$p == 0)
    {
        return $empty_array;
    }
    return _new_v( {
        $VSA_S_KIND  => $S_KIND_ARRAY,
        $VSA_P_AS_AV => $p,
    } );
}

sub v_Array_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Array.
    return $$h->{$VSA_P_AS_AV};
}

###########################################################################

sub v_String
{
    my ($MDLL, $p, $want_cached) = @_;
    if (!ref $p)
    {
        # Expect $p to be a Perl arrayref of non-neg Integer / native ints.
        $p = join q{},
            map { chr(ref $_ ? $MDLL->v_Integer_as_SV($_) : $_) } @$p;
    }
    # Expect $p to be a defined Perl native string, of octets or chars.
    if (exists $_cache->{$S_KIND_STR}->{$p})
    {
        return $_cache->{$S_KIND_STR}->{$p};
    }
    my $h = _new_v( {
        $VSA_S_KIND  => $S_KIND_STR,
        $VSA_P_AS_SV => $p,
    } );
    if ($want_cached)
    {
        $_cache->{$S_KIND_STR}->{$p} = $h;
    }
    return $h;
}

sub v_String_as_SV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an String.
    return $$h->{$VSA_P_AS_SV};
}

sub v_String_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an String.
    return [map { ord $_ } split q{}, $$h->{$VSA_P_AS_SV}];
}

###########################################################################

sub v_Tuple
{
    my ($MDLL, $p) = @_;
    # Expect $p to be a Perl hashref.
    if ((scalar keys %$p) == 0)
    {
        return $nullary_tuple;
    }
    return _new_v( {
        $VSA_S_KIND  => $S_KIND_TUPLE,
        $VSA_P_AS_HV => $p,
    } );
}

sub v_Tuple_as_HV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Tuple.
    return $$h->{$VSA_P_AS_HV};
}

###########################################################################

sub Universal__same # function
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
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
    if ($$h_lhs->{$VSA_S_KIND} eq $$h_rhs->{$VSA_S_KIND})
    {
        return $false;
    }
    my $result_p;
    my $k = $$h_lhs->{$VSA_S_KIND};
    if ($k eq $S_KIND_BOOL)
    {
        confess q{we should never get here due to prior refaddr tests};
    }
    elsif ($k eq $S_KIND_INT)
    {
        $result_p = ($MDLL->v_Integer_as_SV($h_lhs)
            eq $MDLL->v_Integer_as_SV($h_rhs));
    }
    elsif ($k eq $S_KIND_ARRAY)
    {
        confess q{not implemented};
    }
    elsif ($k eq $S_KIND_STR)
    {
        $result_p = ($$h_lhs->{$VSA_P_AS_SV} eq $$h_rhs->{$VSA_P_AS_SV});
    }
    elsif ($k eq $S_KIND_TUPLE)
    {
        confess q{not implemented};
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

sub Universal__assign # updater
{
    my ($MDLL, $var_target, $h_value) = @_;
    $$var_target = $h_value;
}

###########################################################################

sub Boolean__false # function
{
    return $false;
}

sub Boolean__true # function
{
    return $true;
}

sub Boolean__not # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $true) ? $true : $false;
}

sub Boolean__and # function
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true) ? $h_rhs : $false;
}

sub Boolean__or # function
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true) ? $true : $h_rhs;
}

sub Boolean__xor # function
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    return (refaddr $h_lhs == refaddr $true)
        ? $MDLL->Boolean__not($h_rhs) : $h_rhs;
}

###########################################################################

sub Integer__zero # function
{
    return $zero;
}

sub Integer__is_zero # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $zero) ? $true : $false;
}

sub Integer__one # function
{
    return $one;
}

sub Integer__is_one # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $one) ? $true : $false;
}

sub Integer__plus # function
{
    my ($MDLL, $h_augend, $h_addend) = @_;
    if (refaddr $h_augend == refaddr $zero)
    {
        return $h_addend;
    }
    if (refaddr $h_addend == refaddr $zero)
    {
        return $h_augend;
    }
    if (exists $$h_augend->{$VSA_P_AS_BIGINT}
        or exists $$h_addend->{$VSA_P_AS_BIGINT})
    {
        # At least one input is a Math::BigInt object.
        return $MDLL->v_Integer( Math::BigInt->badd(
            exists $$h_augend->{$VSA_P_AS_BIGINT}
                ? $$h_augend->{$VSA_P_AS_BIGINT}
                : $$h_augend->{$VSA_P_AS_SV},
            exists $$h_addend->{$VSA_P_AS_BIGINT}
                ? $$h_addend->{$VSA_P_AS_BIGINT}
                : $$h_addend->{$VSA_P_AS_SV},
        ) );
    }
    # Both inputs are native Perl integers.
    my $sum = $$h_augend->{$VSA_P_AS_SV} + $$h_addend->{$VSA_P_AS_SV};
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $sum ne $sum)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $sum = Math::BigInt->badd( $$h_augend->{$VSA_P_AS_SV},
            $$h_addend->{$VSA_P_AS_SV}
        );
    }
    return $MDLL->v_Integer( $sum );
}

###########################################################################

sub Array__empty # function
{
    return $empty_array;
}

sub Array__is_empty # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $empty_array) ? $true : $false;
}

###########################################################################

sub String__empty # function
{
    return $empty_str;
}

sub String__is_empty # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $empty_str) ? $true : $false;
}

###########################################################################

sub Tuple__nullary # function
{
    return $nullary_tuple;
}

sub Tuple__is_nullary # function
{
    my ($MDLL, $h) = @_;
    return (refaddr $h == refaddr $nullary_tuple) ? $true : $false;
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
