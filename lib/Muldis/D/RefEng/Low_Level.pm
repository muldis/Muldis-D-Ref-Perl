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
    sub _refcount { return B::svref_2object( $_[0] )->REFCNT; };

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
            my $S_KIND_IDENT = 'n';  # value of type Identifier
            my $S_KIND_EXT   = 'e';  # value of type External
        my $VSA_P_AS_SV = 'sv';  # Perl defined nonref scalar if exists
            # Further restriction/detail based on struct kind:
                # Boolean - Perl native boolean; (1==0) or (1==1)
                # Integer - Perl native integer (IV and/or SV)
                # String - Perl native string, either octet or char,
                    # while API to take Muldis D String as Array of Integer exists,
                    # internally always stored as native Perl string,
                    # hence String elements outside Perl's said range not supported
        my $VSA_P_AS_BIGINT = 'bigint';  # Integer - Math::BigInt object if exists
        my $VSA_P_AS_AV = 'av';  # Perl arrayref if exists
            # Further restriction/detail based on struct kind:
                # Array - Perl arrayref of 0..N ::Value objects
                # Capsule (1/2) - Capsule type - same arrayref as for Identifier
                # Identifier - Perl arrayref of 4 elements, in order:
                    # [0] - pkg_name_base - Perl arrayref of 0..N native string
                    # [1] - pkg_name_ext - Perl arrayref of 0..N native string
                    # [2] - rel_starts_n_lev_up - Perl native nonnegative integer
                    # [3] - path_beneath_pkg - Perl arrayref of 0..N native string
        my $VSA_P_AS_HV = 'hv';  # Perl hashref if exists
            # Further restriction/detail based on struct kind:
                # Tuple - Perl hashref of 0..N ::Value objects,
                    # note about String limitations affects Tuple attr names also
                # Capsule (2/2) - Capsule attrs - same hashref as for Tuple
        my $VSA_P_AS_EXT = 'ext';  # External - any native Perl value at all
            # Anything one can assign to a Perl scalar variable / $foo.
            # We store it as is but its a black box to Muldis D at large,
            # and it only makes sense to pass it to/from native Perl code.
            # On matters of value identity:
                # 1. Perl undef equal to itself, unequal to everything else.
                # 2. Two Perl defined non-ref values considered equal iff
                    # their string representations are identical; they are
                    # distinct from undef and all ref/blessed Perl values.
                # 3. Two Perl reference/blessed values considered equal iff
                    # 'refaddr' for both are identical, that is they are
                    # both references to the same thing; appropriate as the
                    # general case of Perl refs/objects is something mutable.
                    # So yes, any ::Value/etc objects will be treated
                    # differently when wrapped by an External ::Value than
                    # they otherwise would, same ref vs same value.
        my $VSA_DICT_C_ELEMS = 'elems';  # the dictionary elements if exists
            # A tree structure conceptually holding set of key+value pairs
            # where the keys all mutually unique and values may not be.
            # Each key+value (DK+DV) typically a pair (DP) of ::Value,
            # lives at a leaf.  A DP is a 2-elem Perl arrayref, [DK,DV].
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
                        # when $k in {Array, Dict}, hval is 2-elem arrayref;
                        # when $k in {External}, hval is 3-elem arrayref.
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
                            # TODO - Consider special-casing treatment when
                            # tuple has a low degree, particularly of 1,
                            # by indexing on attr val rather than serial.
                            # ACTUALLY, higher-level Muldis D code
                            # including the various Relation functions
                            # can explicitly ask for keys/etc on the
                            # specific attrs being eg joined/semijoined on
                            # so normal user code doesn't have to, as S::R,
                            # hence we likely need not this special either.
                        # Each Capsule-typed DK:
                            # hkey is derived from DK "type" attr:
                                # Same as if DK were Identifer-typed.
                            # hval is derived from DK "attrs" attr:
                                # hval is a Perl hashref.
                                # Same format as elems[Tuple].
                            # TODO - Consider special-casing treatment when
                            # attrs has a low degree, particularly of 1,
                            # by indexing on attr val rather than serial.
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
                        # Each External-typed DK:
                            # We are dividing all External into 3 groups.
                            # [0] is a Perl native boolean; it's true if DK
                                # represents the Perl undef, false if not.
                            # [1] is a Perl hashref for DK rep Perl nonref:
                                # hkey is just the DK's EXT payload.
                                # hval is the DP.
                            # [2] is a Perl hashref for DK repr Perl refs:
                                # hkey is refaddr of the DK's EXT payload.
                                # hval is the DP.
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
            # 'keys' and 'indexes' should both be eagerly evaluated.
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
        my $VSA_WHICH = 'which';  # serializes payload if exists
            # Always exists when $k is Identifier.
            # Otherwise only applies to {Array,Tuple,Dictionary,Capsule}
            # and even then only exists conditionally such as when useful
            # to speed up storage in a Dictionary with no 'keys'.
            # When $k is {Tuple,Capsule}, $which is a 2-elem arrayref with
            # the tuple/attrs heading and body serialized separately in
            # elems 0 and 1 respectively, each as Perl native strings;
            # for Capsule we just look at its $s[av] for the balance.
            # When $k is {Array,Dictionary}, whole payload is serialized
            # as a single Perl native string.
            # When $k is {Boolean,Integer,String}, $s[sv] suffices.
            # When $k is External, $s[ext] suffices.
            # Serialization method is inspired by what Set::Relation does.

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

    # External that is Perl undef.
    my $perl_undef_external = _new_v( {
        $VSA_S_KIND   => $S_KIND_EXT,
        $VSA_P_AS_EXT => undef,
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
    return [map { ord $_ } split m//, $$h->{$VSA_P_AS_SV}];
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

sub v_External
{
    my ($MDLL, $p) = @_;
    # Expect $p to be any Perl value at all.
    if (!defined $p)
    {
        return $perl_undef_external;
    }
    return _new_v( {
        $VSA_S_KIND   => $S_KIND_EXT,
        $VSA_P_AS_EXT => $p,
    } );
}

sub v_External_as_Perl
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an External.
    return $$h->{$VSA_P_AS_EXT};
}

###########################################################################

sub Universal__same # function
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    return $MDLL->_same( $h_lhs, $h_rhs ) ? $true : $false;
}

sub _same
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    if (refaddr $h_lhs == refaddr $h_rhs)
    {
        # Caller used the same ::Value object/handle in multiple places.
        return 1;
    }
    if (refaddr $$h_lhs == refaddr $$h_rhs)
    {
        # Caller created 2 ::Value objects/handles likely independently but
        # then probably called same() on them so their structs were merged.
        return 1;
    }
    if ($$h_lhs->{$VSA_S_KIND} eq $$h_rhs->{$VSA_S_KIND})
    {
        return 0;
    }
    my $result_p;
    S_KIND:
    {
        my $k = $$h_lhs->{$VSA_S_KIND};
        if ($k eq $S_KIND_BOOL)
        {
            confess q{we should never get here due to prior refaddr tests};
        }
        if ($k eq $S_KIND_INT)
        {
            $result_p = ($MDLL->v_Integer_as_SV($h_lhs)
                eq $MDLL->v_Integer_as_SV($h_rhs));
            last S_KIND;
        }
        if ($k eq $S_KIND_ARRAY)
        {
            my $lhs_av = $$h_lhs->{$VSA_P_AS_AV};
            my $rhs_av = $$h_rhs->{$VSA_P_AS_AV};
            if (scalar @{$lhs_av} != scalar @{$rhs_av})
            {
                # Arrays have different elem count.
                $result_p = 0;
                last S_KIND;
            }
            for my $i (0..$#{$lhs_av})
            {
                if (!$MDLL->_same( $lhs_av->[$i], $rhs_av->[$i] ))
                {
                    # Values of corresponding array elems aren't same.
                    $result_p = 0;
                    last S_KIND;
                }
            }
            $result_p = 1;
            last S_KIND;
        }
        if ($k eq $S_KIND_STR)
        {
            $result_p = ($$h_lhs->{$VSA_P_AS_SV} eq $$h_rhs->{$VSA_P_AS_SV});
            last S_KIND;
        }
        if ($k eq $S_KIND_DICT)
        {
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_TUPLE)
        {
            my $lhs_hv = $$h_lhs->{$VSA_P_AS_HV};
            my $rhs_hv = $$h_rhs->{$VSA_P_AS_HV};
            if ((scalar keys %{$lhs_hv}) != (scalar keys %{$rhs_hv}))
            {
                # Tuples have different attribute count.
                $result_p = 0;
                last S_KIND;
            }
            for my $atnm (keys %{$lhs_hv})
            {
                if (!exists $rhs_hv->{$atnm})
                {
                    # Tuples have at least 1 differently-named attribute.
                    $result_p = 0;
                    last S_KIND;
                }
            }
            for my $atnm (keys %{$lhs_hv})
            {
                if (!$MDLL->_same( $lhs_hv->{$atnm}, $rhs_hv->{$atnm} ))
                {
                    # Values of corresponding tuple attrs aren't same.
                    $result_p = 0;
                    last S_KIND;
                }
            }
            $result_p = 1;
            last S_KIND;
        }
        if ($k eq $S_KIND_CPSL)
        {
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_IDENT)
        {
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_EXT)
        {
            if (!defined $$h_lhs->{$VSA_P_AS_EXT}
                and !defined $$h_rhs->{$VSA_P_AS_EXT})
            {
                confess q{we should never get here due to prior refaddr tests};
            }
            if (!defined $$h_lhs->{$VSA_P_AS_EXT}
                or !defined $$h_rhs->{$VSA_P_AS_EXT})
            {
                # One input is Perl undef and other is not.
                $result_p = 0;
                last S_KIND;
            }
            if (!ref $$h_lhs->{$VSA_P_AS_EXT} or !ref $$h_rhs->{$VSA_P_AS_EXT})
            {
                # Same if both inputs equal Perl non-ref strings.
                $result_p = (!ref $$h_lhs->{$VSA_P_AS_EXT}
                    and !ref $$h_rhs->{$VSA_P_AS_EXT}
                    and $$h_lhs->{$VSA_P_AS_EXT} eq $$h_rhs->{$VSA_P_AS_EXT});
                last S_KIND;
            }
            # Same if both inputs are the same Perl reference.
            $result_p = (refaddr $$h_lhs->{$VSA_P_AS_EXT}
                == refaddr $$h_rhs->{$VSA_P_AS_EXT});
            last S_KIND;
        }
        confess q{we should never get here};
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
    return $result_p;
}

sub Universal__assign # updater
{
    my ($MDLL, $var_target, $h_value) = @_;
    $$var_target = $h_value;
    return;
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
