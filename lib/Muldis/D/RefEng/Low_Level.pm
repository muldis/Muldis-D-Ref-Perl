use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use B;
use Math::BigInt try => 'GMP';

# Note that many Math::BigInt methods mutate their invocants, so for those
# we either need to say $x->copy()->foo(...) or Math::BigInt->foo($x,...).

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

    # CONSTANTS:
    my $MD_PKG_NAME = q{::Muldis_D:"http://muldis.com":"0.200"};

    my $LARGE_STR_THRESH = 1000;

    # NAMING CONVENTIONS:
        # $MDLL : singleton obj repr RefEng::Low_Level class
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
            my $S_KIND_BAG   = 'm';  # value of type Bag
            my $S_KIND_TUPLE = 't';  # value of type Tuple
            my $S_KIND_CPSL  = 'c';  # value of type Capsule
            my $S_KIND_IDENT = 'n';  # value of type SC_Identifier
            my $S_KIND_EXT   = 'e';  # value of type External
        my $VSA_SUBTYPE = 'subtype';  # declares main value subtype if exists
            # This is a cache of information gleanable from $p itself.
            # Allowed options depend on the struct kind:
            # Bag: set,relation,other
            # SC_Identifier:
                my $IDENT_ST_IDENTITY = 'identity';  # all Capsule are this
                my $IDENT_ST_ABSOLUTE = 'absolute';
                my $IDENT_ST_RELATIVE = 'relative';
                my $IDENT_ST_FLOATING = 'floating';
        my $VSA_WHICH = 'which';  # serializes payload if exists
            # Serialization takes the form of a Muldis D Plain_Text value
            # literal for the value in question, in a canonical format so
            # two ::Value with the same 'which' are guaranteed to be the
            # same value as far as Muldis D is concerned.
            # Note that $s[which] is at least subtly different than both
            # $s[scalar] and $s[ident][4] for the requisite types.
            # Moreover, it will use special syntax for a variety of special
            # cases that are subtypes of Capsule, for example
            # {Blob,Text,Ratio,Float,Quantity,Interval,Set,Relation} and
            # {Dictionary,SC_Heading,SC_Renaming} and
            # {SC_Func_Args,SC_Func_Params,SC_Proc_Args,SC_Proc_Params}
            # etc have their own Plain_Text syntax.
        my $VSA_SCALAR = 'scalar';  # Perl defined nonref scalar if exists
            # Further restriction/detail based on struct kind:
                # Boolean - Perl native boolean; (1==0) or (1==1)
                # Integer - Perl native integer (IV and/or SV)
                # String - Perl native string, either octet or char,
                    # while API to take Muldis D String as Array of Integer exists,
                    # internally always stored as native Perl string,
                    # hence String elements outside Perl's said range not supported
        my $VSA_BIGINT = 'bigint';  # Integer - Math::BigInt object if exists
        my $VSA_ARRAY = 'array';  # Array - Perl arrayref if exists of 0..N ::Value objects
        my $VSA_BAG = 'bag';  # Bag - Perl hashref if exists of 5 elements:
            my $BAG_C_ELEMS_BY_R = 'elems_by_r';  # Perl hashref of 0..N QV,
                # where a QV (quantified value) is a 2-elem Perl arrayref,
                # its elements in order are UV (unique value, a ::Value object)
                # and VQ (value quantity, a Perl native positive integer).
                # Each hashref element represents one Bag element:
                    # hkey is refaddr of the value struct / $s of the UV ::Value.
                    # hval is the QV itself.
                # While conceptually every UV is unique within a Bag,
                # there may exist multiple QV with the same UV since duplicate
                # merging is lazy for performance reasons, as hashing or
                # comparing the actual values to ensure uniqueness may be
                # expensive for some value types.
            my $BAG_C_IKDF = 'ikdf';  # is_known_dup_free - Perl native boolean
                # This is true if 'elems' is known to have no duplicate UV;
                # it is false by default unless the Bag is empty.
            my $BAG_C_ELEMS_BY_V = 'elems_by_v';  # Perl hashref if exists of 0..N QV,
                # like 'elems' but each hkey is 'which' of UV rather than
                # refaddr, and hence the Bag has no duplicate UV.
            my $BAG_C_INDEXES = 'indexes';  # TODO, hashref and see Set::Relation::V2.
            my $BAG_C_KEYS = 'keys';  # TODO, hashref and see Set::Relation::V2.
            # TODO: In the future we could make the 'bag' structure more
            # complicated such as like a B+tree if performance warrants it,
            # but for now the current design is much simpler to implement.
        my $VSA_TUPLE = 'tuple';  # Tuple, Capsule (attrs;2/2), for both:
            # Perl hashref if exists of 0..N ::Value objects,
                # note about String limitations affects Tuple attr names also
        my $VSA_IDENT = 'ident';  # SC_Identifier, Capsule (type;1/2), for both:
            # Perl arrayref if exists of 5 elements, in order:
                # [0] - pkg_name_base - Perl arrayref of 0..N native string
                # [1] - pkg_name_ext - Perl arrayref of 0..N native string
                # [2] - rel_starts_n_lev_up - Perl native nonnegative integer
                # [3] - path_beneath_pkg - Perl arrayref of 0..N native string
                # [4] - Perl native string that caches serialize of [0..3].
                # TODO - make SC_Identifier payload a hashref instead maybe
        my $VSA_EXT = 'ext';  # External - any native Perl value at all
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

    # ATTRIBUTE LIST OF ::LowLevel OBJECTS:
        # TODO

    my $_MDLL = bless {}, __PACKAGE__;

    my $_cache = {
        # For these types, all values of the type are cached here.
        $S_KIND_BOOL  => {},
        $S_KIND_IDENT => {},
        # For these types, just some of the values are cached here.
        $S_KIND_INT   => {},
        $S_KIND_STR   => {},
        # For these types, none of the values are cached here, but we
        # maintain an empty hashref so some other code can be simpler.
        # TODO - there is no such code yet.
        $S_KIND_ARRAY => {},
        $S_KIND_BAG   => {},
        $S_KIND_TUPLE => {},
        $S_KIND_CPSL  => {},
        $S_KIND_EXT   => {},
    };

    # Boolean false (type default val) and true values.
    my $false = $_cache->{$S_KIND_BOOL}->{(1==0)} = _new_v( {
        $VSA_S_KIND => $S_KIND_BOOL,
        $VSA_WHICH  => 'false',
        $VSA_SCALAR => (1==0),
    } );
    my $true = $_cache->{$S_KIND_BOOL}->{(1==1)} = _new_v( {
        $VSA_S_KIND => $S_KIND_BOOL,
        $VSA_WHICH  => 'true',
        $VSA_SCALAR => (1==1),
    } );

    # Integer 0 (type default val) and 1 and -1 values.
    my $zero = $_cache->{$S_KIND_INT}->{0} = _new_v( {
        $VSA_S_KIND => $S_KIND_INT,
        $VSA_WHICH  => '0',
        $VSA_SCALAR => 0,
    } );
    my $one = $_cache->{$S_KIND_INT}->{1} = _new_v( {
        $VSA_S_KIND => $S_KIND_INT,
        $VSA_WHICH  => '1',
        $VSA_SCALAR => 1,
    } );
    my $neg_one = $_cache->{$S_KIND_INT}->{'-1'} = _new_v( {
        $VSA_S_KIND => $S_KIND_INT,
        $VSA_WHICH  => '-1',
        $VSA_SCALAR => -1,
    } );

    # Array with no elements (type default val).
    my $empty_array = _new_v( {
        $VSA_S_KIND => $S_KIND_ARRAY,
        $VSA_WHICH  => '[]',
        $VSA_ARRAY  => [],
    } );

    # String with no elements (type default val).
    my $empty_str = $_cache->{$S_KIND_STR}->{0} = _new_v( {
        $VSA_S_KIND => $S_KIND_STR,
        $VSA_WHICH  => q{\\+[]},
        $VSA_SCALAR => q{},
    } );

    # Bag with no elements (type default val).
    my $empty_bag = _new_v( {
        $VSA_S_KIND => $S_KIND_BAG,
        $VSA_WHICH  => '\\+{}',
        $VSA_BAG    => {
            $BAG_C_ELEMS_BY_R => {},
            $BAG_C_IKDF       => 1,
            $BAG_C_ELEMS_BY_V => {},
            $BAG_C_INDEXES    => {},
            $BAG_C_KEYS       => {},
        },
    } );

    # Tuple with no elements (type default val).
    my $nullary_tuple = _new_v( {
        $VSA_S_KIND => $S_KIND_TUPLE,
        $VSA_WHICH  => '\\%{}',
        $VSA_TUPLE  => {},
    } );

    # SC_Identifier (relative) meaning "self" (type default val).
    my $self_ident = _new_v( {
        $VSA_S_KIND  => $S_KIND_IDENT,
        $VSA_SUBTYPE => $IDENT_ST_RELATIVE,
        $VSA_WHICH   => '\\$0',
        $VSA_IDENT   => [[],[],0,[],'0'],
    } );

    # External that is Perl undef (type default val).
    my $perl_undef_external = _new_v( {
        $VSA_S_KIND => $S_KIND_EXT,
        $VSA_WHICH  => q{\\~external~'undef'},
        $VSA_EXT    => undef,
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
    return $$h->{$VSA_SCALAR};
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
            $VSA_S_KIND => $S_KIND_INT,
            $VSA_SCALAR => $p,
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
        if ($p->is_one('-'))
        {
            return $neg_one;
        }
        my $h = _new_v( {
            $VSA_S_KIND => $S_KIND_INT,
            $VSA_BIGINT => $p,
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
    if (!exists $$h->{$VSA_SCALAR})
    {
        $$h->{$VSA_SCALAR} = $$h->{$VSA_BIGINT}->bstr();
    }
    return $$h->{$VSA_SCALAR};
}

sub v_Integer_as_BigInt
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Integer.
    if (!exists $$h->{$VSA_BIGINT})
    {
        $$h->{$VSA_BIGINT} = Math::BigInt->new( $$h->{$VSA_SCALAR} );
    }
    return $$h->{$VSA_BIGINT};
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
        $VSA_S_KIND => $S_KIND_ARRAY,
        $VSA_ARRAY  => $p,
    } );
}

sub v_Array_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Array.
    return $$h->{$VSA_ARRAY};
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
    if ((length $p) <= $LARGE_STR_THRESH
        and exists $_cache->{$S_KIND_STR}->{$p})
    {
        return $_cache->{$S_KIND_STR}->{$p};
    }
    my $h = _new_v( {
        $VSA_S_KIND => $S_KIND_STR,
        $VSA_SCALAR => $p,
    } );
    if ($want_cached and (length $p) <= $LARGE_STR_THRESH)
    {
        $_cache->{$S_KIND_STR}->{$p} = $h;
    }
    return $h;
}

sub v_String_as_SV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an String.
    return $$h->{$VSA_SCALAR};
}

sub v_String_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an String.
    return [map { ord $_ } split m//, $$h->{$VSA_SCALAR}];
}

###########################################################################

sub v_Bag
{
    my ($MDLL, $p) = @_;
    # Expect $p to be a Perl arrayref of 2-element Perl arrayrefs.
    if (@$p == 0)
    {
        return $empty_bag;
    }
    return _new_v( {
        $VSA_S_KIND => $S_KIND_BAG,
        $VSA_BAG    => {
            $BAG_C_ELEMS_BY_R => {map {((refaddr ${$_->[0]}) => $_)} @$p},
            $BAG_C_IKDF       => 0,
            $BAG_C_INDEXES    => {},
            $BAG_C_KEYS       => {},
        },
    } );
}

sub v_Bag_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Bag.
    return [values %{$$h->{$VSA_BAG}->{$BAG_C_ELEMS_BY_R}}];
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
        $VSA_S_KIND => $S_KIND_TUPLE,
        $VSA_TUPLE  => $p,
    } );
}

sub v_Tuple_as_HV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Tuple.
    return $$h->{$VSA_TUPLE};
}

###########################################################################

sub v_Capsule
{
    my ($MDLL, $p_type, $p_attrs) = @_;
    # Expect $p_type to be a Perl arrayref and $p_attrs a Perl hashref.
    return $MDLL->_select_Capsule(
        $MDLL->v_SC_Identifier( $p_type ), $MDLL->v_Tuple( $p_attrs ) );
}

sub v_Capsule_type_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Capsule.
    return $MDLL->v_SC_Identifier_as_AV( $MDLL->Capsule__Capsule_type( $h ) );
}

sub v_Capsule_attrs_as_HV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an Capsule.
    return $MDLL->v_Tuple_as_HV( $MDLL->Capsule__attrs( $h ) );
}

###########################################################################

sub v_SC_Identifier
{
    my ($MDLL, $p) = @_;
    # Expect $p to be a Perl arrayref of 4 elements.
    my ($pkg_name_base, $pkg_name_ext, $rel_starts_n_lev_up,
        $path_beneath_pkg) = @{$p};

    # - There are 4 kinds of SC_Identifier that are all mutually disjoint:
    #     - identity : when +base, +ext, -levels, -path
    #               or when +base, +ext, -levels, +path
    #     - absolute : when +base, -ext, -levels, -path
    #               or when +base, -ext, -levels, +path
    #     - relative : when -base, -ext, -levels, -path
    #               or when -base, -ext, +levels, +path
    #     - floating : when -base, -ext, -levels, +path
    # - For base,ext,path : + means nonempty, - means empty
    # - For levels        : + means nonzero , - means zero
    # - The other 9 possible combos of SC_Identifier elems are illegal.
    # - The relative val of 4x- means "self", is SC_Identifier default val.
    # - The identity subtype is the full declared name of the package
    #       actually linked in, not what was requested by users.
    # - A Capsule specifically requires "identity" SC_Identifiers for "type",
    #       and proper subtype Reference has all such "identity" values.

    confess q{illegal SC_Identifier}
        if $rel_starts_n_lev_up < 0;

    my $subtype;
    my $serial;
    if (scalar @{$pkg_name_base} and $rel_starts_n_lev_up != 0)
    {
        # 4/16 combos
        confess q{illegal SC_Identifier};
    }
    elsif (scalar @{$pkg_name_base})
    {
        # 4/16 combos
        $subtype = (scalar @{$pkg_name_ext})
            ? $IDENT_ST_IDENTITY : $IDENT_ST_ABSOLUTE;
        $serial = join q{},
            (map { q{::}._normalize_name_str($_) } @{$pkg_name_base}),
            (map { q{:}._normalize_name_str($_) } @{$pkg_name_ext}),
            (map { q{.}._normalize_name_str($_) } @{$path_beneath_pkg});
    }
    elsif (scalar @{$pkg_name_ext})
    {
        # 4/16 combos
        confess q{illegal SC_Identifier};
    }
    elsif (scalar @{$path_beneath_pkg})
    {
        # 2/16 combos
        my $subtype = $rel_starts_n_lev_up != 0
            ? $IDENT_ST_RELATIVE : $IDENT_ST_FLOATING;
        my $serial = join q{.},
            ($rel_starts_n_lev_up != 0 ? $rel_starts_n_lev_up : ()),
            (map { _normalize_name_str($_) } @{$path_beneath_pkg});
    }
    elsif ($rel_starts_n_lev_up == 0)
    {
        # 1/16 combos
        # $subtype = $IDENT_ST_RELATIVE, $serial = '0'
        return $self_ident;
    }
    else
    {
        # 1/16 combos
        confess q{illegal SC_Identifier};
    }

    my $av = [$pkg_name_base, $pkg_name_ext, $rel_starts_n_lev_up,
        $path_beneath_pkg, $serial];

    if (exists $_cache->{$S_KIND_IDENT}->{$serial})
    {
        return $_cache->{$S_KIND_IDENT}->{$serial};
    }
    my $h = _new_v( {
        $VSA_S_KIND  => $S_KIND_IDENT,
        $VSA_SUBTYPE => $subtype,
        $VSA_WHICH   => '\\$' . $serial,
        $VSA_IDENT   => $av,
    } );
    $_cache->{$S_KIND_IDENT}->{$serial} = $h;
    return $h;
}

sub _normalize_name_str
{
    my ($str) = @_;
    if ($str !~ m/^[A-Z_][A-Z_a-z0-9]*$/)
    {
        $str =~ s/\\/\\\\/;
        $str =~ s/"/\\"/;
        $str = qq{"$str"};
    }
    return $str;
}

sub v_SC_Identifier_as_AV
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an SC_Identifier.
    return [@{$$h->{$VSA_IDENT}}[0..3]];
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
        $VSA_S_KIND => $S_KIND_EXT,
        $VSA_EXT    => $p,
    } );
}

sub v_External_as_Perl
{
    my ($MDLL, $h) = @_;
    # Expect $h to be an External.
    return $$h->{$VSA_EXT};
}

###########################################################################

sub Universal__same # function
{
    my ($MDLL, $topic) = @_;
    # Expect $topic is a Perl arrayref.
    my ($h_lhs, $h_rhs) = @{$topic};
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
            my $lhs_av = $$h_lhs->{$VSA_ARRAY};
            my $rhs_av = $$h_rhs->{$VSA_ARRAY};
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
            $result_p = ($$h_lhs->{$VSA_SCALAR} eq $$h_rhs->{$VSA_SCALAR});
            last S_KIND;
        }
        if ($k eq $S_KIND_BAG)
        {
            # Note that for now we are ignoring any possible optimizations
            # that may come from the presence of 'keys'/'indexes'.
            # In order to know if 2 Bag are the same we must first ensure
            # that any duplicate UV in elems_by_r are merged, and we do
            # that most simply by ensuring elems_by_v is populated,
            # which as a side-effect will deduplicate elems_by_r also.
            my $lhs_bag = $$h_lhs->{$VSA_BAG};
            my $rhs_bag = $$h_rhs->{$VSA_BAG};
            if (!exists $lhs_bag->{$BAG_C_ELEMS_BY_V})
            {
                $MDLL->_want_bag_elems_by_v( $lhs_bag );
            }
            if (!exists $rhs_bag->{$BAG_C_ELEMS_BY_V})
            {
                $MDLL->_want_bag_elems_by_v( $rhs_bag );
            }
            my $lhs_ebv = $lhs_bag->{$BAG_C_ELEMS_BY_V};
            my $rhs_ebv = $rhs_bag->{$BAG_C_ELEMS_BY_V};
            if ((scalar values %{$lhs_ebv}) != (scalar values %{$rhs_ebv}))
            {
                # Bags have different numbers of quantified values.
                $result_p = 0;
                last S_KIND;
            }
            for my $uvw (keys %{$lhs_ebv})
            {
                if (!exists $rhs_ebv->{$uvw})
                {
                    # Unique value doesn't exist in both bags.
                    $result_p = 0;
                    last S_KIND;
                }
                if ($lhs_ebv->{$uvw}->[1] ne $rhs_ebv->{$uvw}->[1])
                {
                    # Quantities of corresponding unique values not same.
                    $result_p = 0;
                    last S_KIND;
                }
                # TODO: Even if the 2 Bag as a whole don't end up being
                # considered the same, merge the structures of matching
                # elements we come across to save memory.
            }
            $result_p = 1;
            last S_KIND;
        }
        if ($k eq $S_KIND_TUPLE)
        {
            $result_p = $MDLL->_same_tuple( $h_lhs, $h_rhs );
            last S_KIND;
        }
        if ($k eq $S_KIND_CPSL)
        {
            $result_p = (($$h_lhs->{$VSA_IDENT}->[4]
                    eq $$h_rhs->{$VSA_IDENT}->[4])
                and $MDLL->_same_tuple( $h_lhs, $h_rhs ));
            last S_KIND;
        }
        if ($k eq $S_KIND_IDENT)
        {
            $result_p = ($$h_lhs->{$VSA_IDENT}->[4]
                eq $$h_rhs->{$VSA_IDENT}->[4]);
            last S_KIND;
        }
        if ($k eq $S_KIND_EXT)
        {
            if (!defined $$h_lhs->{$VSA_EXT}
                and !defined $$h_rhs->{$VSA_EXT})
            {
                confess q{we should never get here due to prior refaddr tests};
            }
            if (!defined $$h_lhs->{$VSA_EXT}
                or !defined $$h_rhs->{$VSA_EXT})
            {
                # One input is Perl undef and other is not.
                $result_p = 0;
                last S_KIND;
            }
            if (!ref $$h_lhs->{$VSA_EXT} or !ref $$h_rhs->{$VSA_EXT})
            {
                # Same if both inputs equal Perl non-ref strings.
                $result_p = (!ref $$h_lhs->{$VSA_EXT}
                    and !ref $$h_rhs->{$VSA_EXT}
                    and $$h_lhs->{$VSA_EXT} eq $$h_rhs->{$VSA_EXT});
                last S_KIND;
            }
            # Same if both inputs are the same Perl reference.
            $result_p = (refaddr $$h_lhs->{$VSA_EXT}
                == refaddr $$h_rhs->{$VSA_EXT});
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
        # TODO, that may not be the one in $_cache if applicable though for
        # types where said caching is optional.
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

sub _same_tuple
{
    my ($MDLL, $h_lhs, $h_rhs) = @_;
    my $lhs_hv = $$h_lhs->{$VSA_TUPLE};
    my $rhs_hv = $$h_rhs->{$VSA_TUPLE};
    if ((scalar keys %{$lhs_hv}) != (scalar keys %{$rhs_hv}))
    {
        # Tuples have different attribute count.
        return 0;
    }
    for my $atnm (keys %{$lhs_hv})
    {
        if (!exists $rhs_hv->{$atnm})
        {
            # Tuples have at least 1 differently-named attribute.
            return 0;
        }
    }
    for my $atnm (keys %{$lhs_hv})
    {
        if (!$MDLL->_same( $lhs_hv->{$atnm}, $rhs_hv->{$atnm} ))
        {
            # Values of corresponding tuple attrs aren't same.
            return 0;
        }
    }
    return 1;
}

sub _want_bag_elems_by_v
{
    confess qq{not implemented};
}

###########################################################################

sub _which
{
    my ($MDLL, $h) = @_;
    if (exists $$h->{$VSA_WHICH})
    {
        return $$h->{$VSA_WHICH};
    }
    my $which;
    S_KIND:
    {
        my $k = $$h->{$VSA_S_KIND};
        if ($k eq $S_KIND_BOOL)
        {
            confess q{we should never get here due to prior exists tests};
        }
        if ($k eq $S_KIND_INT)
        {
            $which = $$h->{$VSA_SCALAR};
            last S_KIND;
        }
        if ($k eq $S_KIND_ARRAY)
        {
            $which = '[' . (
                join q{,}, map { $MDLL->_which($_) } @{$$h->{$VSA_ARRAY}}
                ) . ']';
            last S_KIND;
        }
        if ($k eq $S_KIND_STR)
        {
            $which = '\\+['.(join q{,}, @{$MDLL->v_String_as_AV($h)}).']';
            last S_KIND;
        }
        if ($k eq $S_KIND_BAG)
        {
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_TUPLE)
        {
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_CPSL)
        {
            my $subtype = $$h->{$VSA_IDENT}->[4];
            if ($subtype eq $MD_PKG_NAME.'.Blob')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Text')
            {
                my $h_maximal_chars = $$h->{$VSA_TUPLE}->{maximal_chars};
                $which = $$h_maximal_chars->{$VSA_SCALAR};
                $which =~ s/\\/\\\\/;
                $which =~ s/'/\\'/;
                $which = qq{'$which'};
                last S_KIND;
            }
            if ($subtype eq $MD_PKG_NAME.'.Ratio')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Float')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Quantity')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Interval')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Set')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Relation')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.Dictionary')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Heading')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Renaming')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Func_Args')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Func_Params')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Proc_Args')
            {
                confess qq{$k subtype not implemented};
            }
            if ($subtype eq $MD_PKG_NAME.'.SC_Proc_Params')
            {
                confess qq{$k subtype not implemented};
            }
            confess qq{$k not implemented};
        }
        if ($k eq $S_KIND_IDENT)
        {
            confess q{we should never get here due to prior exists tests};
        }
        if ($k eq $S_KIND_EXT)
        {
            confess qq{$k not implemented};
        }
        confess q{we should never get here};
    }
    return $$h->{$VSA_WHICH} = $which;
}

###########################################################################

sub Universal__assign # updater
{
    my ($MDLL, $var_topic) = @_;
    # Expect $var_topic is a Perl arrayref.
    my ($var_target, $h_value) = @{$var_topic};
    $$var_target = $h_value;
    return;
}

###########################################################################

sub Boolean__false # constant
{
    return $false;
}

sub Boolean__true # constant
{
    return $true;
}

sub Boolean__not # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $true) ? $true : $false;
}

sub Boolean__and # function
{
    my ($MDLL, $topic) = @_;
    my ($h_lhs, $h_rhs) = @{$topic};
    return (refaddr $h_lhs == refaddr $true) ? $h_rhs : $false;
}

sub Boolean__or # function
{
    my ($MDLL, $topic) = @_;
    my ($h_lhs, $h_rhs) = @{$topic};
    return (refaddr $h_lhs == refaddr $true) ? $true : $h_rhs;
}

sub Boolean__xor # function
{
    my ($MDLL, $topic) = @_;
    my ($h_lhs, $h_rhs) = @{$topic};
    return (refaddr $h_lhs == refaddr $true)
        ? $MDLL->Boolean__not($h_rhs) : $h_rhs;
}

###########################################################################

sub Integer__zero # constant
{
    return $zero;
}

sub Integer__is_zero # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $zero) ? $true : $false;
}

sub Integer__one # constant
{
    return $one;
}

sub Integer__is_one # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $one) ? $true : $false;
}

sub Integer__neg_one # constant
{
    return $neg_one;
}

sub Integer__is_neg_one # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $neg_one) ? $true : $false;
}

sub Integer__is_neg # function
{
    my ($MDLL, $h_topic) = @_;
    if (exists $$h_topic->{$VSA_BIGINT})
    {
        return $$h_topic->{$VSA_BIGINT}->is_neg() ? $true : $false;
    }
    return ($$h_topic->{$VSA_SCALAR} < 0) ? $true : $false;
}

sub Integer__pred # function
{
    my ($MDLL, $h_topic) = @_;
    if (exists $$h_topic->{$VSA_BIGINT})
    {
        return $MDLL->v_Integer( $$h_topic->{$VSA_BIGINT}->copy()->bdec() );
    }
    my $res = $$h_topic->{$VSA_SCALAR} - 1;
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $res ne $res)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $res = Math::BigInt->new( $$h_topic->{$VSA_SCALAR} )->bdec();
    }
    return $MDLL->v_Integer( $res );
}

sub Integer__succ # function
{
    my ($MDLL, $h_topic) = @_;
    if (exists $$h_topic->{$VSA_BIGINT})
    {
        return $MDLL->v_Integer( $$h_topic->{$VSA_BIGINT}->copy()->binc() );
    }
    my $res = $$h_topic->{$VSA_SCALAR} + 1;
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $res ne $res)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $res = Math::BigInt->new( $$h_topic->{$VSA_SCALAR} )->binc();
    }
    return $MDLL->v_Integer( $res );
}

sub Integer__opposite # function
{
    my ($MDLL, $h_topic) = @_;
    if (refaddr $h_topic == refaddr $zero)
    {
        return $h_topic;
    }
    if (exists $$h_topic->{$VSA_BIGINT})
    {
        return $MDLL->v_Integer( $$h_topic->{$VSA_BIGINT}->copy()->bneg() );
    }
    # Process in string form having full precision; remove or add the '-'.
    return $MDLL->v_Integer( substr($$h_topic->{$VSA_SCALAR},0,1) eq '-'
        ? substr($$h_topic->{$VSA_SCALAR},1)
        : '-'.$$h_topic->{$VSA_SCALAR} );
}

sub Integer__abs # function
{
    my ($MDLL, $h_topic) = @_;
    if (!$MDLL->Integer__is_neg( $h_topic ))
    {
        return $h_topic;
    }
    if (exists $$h_topic->{$VSA_BIGINT})
    {
        return $MDLL->v_Integer( $$h_topic->{$VSA_BIGINT}->copy()->babs() );
    }
    # Process in string form having full precision; remove the '-'.
    return $MDLL->v_Integer( substr($$h_topic->{$VSA_SCALAR},1) );
}

sub Integer__plus # function
{
    my ($MDLL, $topic) = @_;
    my ($h_augend, $h_addend) = @{$topic};
    return $MDLL->_plus( $h_augend, $h_addend );
}

sub _plus
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
    if (refaddr $h_augend == refaddr $one)
    {
        return $MDLL->Integer__succ( $h_addend );
    }
    if (refaddr $h_addend == refaddr $one)
    {
        return $MDLL->Integer__succ( $h_augend );
    }
    if (refaddr $h_augend == refaddr $neg_one)
    {
        return $MDLL->Integer__pred( $h_addend );
    }
    if (refaddr $h_addend == refaddr $neg_one)
    {
        return $MDLL->Integer__pred( $h_augend );
    }
    if (exists $$h_augend->{$VSA_BIGINT})
    {
        # At least the first input is a Math::BigInt object.
        return $MDLL->v_Integer(
            $$h_augend->{$VSA_BIGINT}->copy()->badd(
                exists $$h_addend->{$VSA_BIGINT}
                ? $$h_addend->{$VSA_BIGINT}
                : $$h_addend->{$VSA_SCALAR} ) );
    }
    if (exists $$h_addend->{$VSA_BIGINT})
    {
        # Just the second input is a Math::BigInt object.
        return $MDLL->v_Integer(
            $$h_addend->{$VSA_BIGINT}->copy()->badd(
                $$h_augend->{$VSA_SCALAR} ) );
    }
    # Both inputs are native Perl integers.
    my $sum = $$h_augend->{$VSA_SCALAR} + $$h_addend->{$VSA_SCALAR};
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $sum ne $sum)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $sum = Math::BigInt->badd( $$h_augend->{$VSA_SCALAR},
            $$h_addend->{$VSA_SCALAR}
        );
    }
    return $MDLL->v_Integer( $sum );
}

sub Integer__minus # function
{
    my ($MDLL, $topic) = @_;
    my ($h_minuend, $h_subtrahend) = @{$topic};
    return $MDLL->_plus( $h_minuend,
        $MDLL->Integer__opposite( $h_subtrahend ) );
}

sub Integer__abs_minus # function
{
    my ($MDLL, $topic) = @_;
    my ($h_minuend, $h_subtrahend) = @{$topic};
    return $MDLL->Integer__abs( $MDLL->_plus( $h_minuend,
        $MDLL->Integer__opposite( $h_subtrahend ) ) );
}

sub Integer__times # function
{
    my ($MDLL, $topic) = @_;
    my ($h_multiplicand, $h_multiplier) = @{$topic};
    if (refaddr $h_multiplicand == refaddr $one)
    {
        return $h_multiplier;
    }
    if (refaddr $h_multiplier == refaddr $one)
    {
        return $h_multiplicand;
    }
    if (refaddr $h_multiplicand == refaddr $zero
        or refaddr $h_multiplier == refaddr $zero)
    {
        return $zero;
    }
    if (refaddr $h_multiplicand == refaddr $neg_one)
    {
        return $MDLL->Integer__opposite( $h_multiplier );
    }
    if (refaddr $h_multiplier == refaddr $neg_one)
    {
        return $MDLL->Integer__opposite( $h_multiplicand );
    }
    if (exists $$h_multiplicand->{$VSA_BIGINT})
    {
        # At least the first input is a Math::BigInt object.
        return $MDLL->v_Integer(
            $$h_multiplicand->{$VSA_BIGINT}->copy()->bmul(
                exists $$h_multiplier->{$VSA_BIGINT}
                ? $$h_multiplier->{$VSA_BIGINT}
                : $$h_multiplier->{$VSA_SCALAR} ) );
    }
    if (exists $$h_multiplier->{$VSA_BIGINT})
    {
        # Just the second input is a Math::BigInt object.
        return $MDLL->v_Integer(
            $$h_multiplier->{$VSA_BIGINT}->copy()->bmul(
                $$h_multiplicand->{$VSA_SCALAR} ) );
    }
    # Both inputs are native Perl integers.
    my $prod = $$h_multiplicand->{$VSA_SCALAR} + $$h_multiplier->{$VSA_SCALAR};
    # We actually want to use a devel tool to see if the IV is canonical rather than an FV.
    if (int $prod ne $prod)
    {
        # Result too big for an IV so we lost precision and got an FV.
        $prod = Math::BigInt->bmul( $$h_multiplicand->{$VSA_SCALAR},
            $$h_multiplier->{$VSA_SCALAR}
        );
    }
    return $MDLL->v_Integer( $prod );
}

sub Integer__whole_divide_rtz # function
{
    my ($MDLL, $topic) = @_;
    my ($h_dividend, $h_divisor) = @{$topic};
    return $MDLL->_divide_and_modulo_rtz( $h_dividend, $h_divisor )->[0];
}

sub Integer__modulo_rtz # function
{
    my ($MDLL, $topic) = @_;
    my ($h_dividend, $h_divisor) = @{$topic};
    return $MDLL->_divide_and_modulo_rtz( $h_dividend, $h_divisor )->[1];
}

sub Integer__divide_and_modulo_rtz # function
{
    my ($MDLL, $topic) = @_;
    my ($h_dividend, $h_divisor) = @{$topic};
    return $MDLL->v_Array(
        $MDLL->_divide_and_modulo_rtz( $h_dividend, $h_divisor ) );
}

sub _divide_and_modulo_rtz # function
{
    my ($MDLL, $h_dividend, $h_divisor) = @_;
    if (refaddr $h_divisor == refaddr $zero)
    {
        confess q{illegal divisor of zero};
    }
    if (refaddr $h_dividend == refaddr $zero)
    {
        return $zero;
    }
    if (refaddr $h_divisor == refaddr $one)
    {
        return $h_dividend;
    }
    if ($MDLL->_same( $h_dividend, $h_divisor ))
    {
        return $one;
    }
    if (refaddr $h_divisor == refaddr $neg_one)
    {
        return $MDLL->Integer__opposite( $h_dividend );
    }
    confess q{unimplemented};
}

sub Integer__power # function
{
    my ($MDLL, $topic) = @_;
    my ($h_radix, $h_exponent) = @{$topic};
    confess q{unimplemented};
}

sub Integer__factorial # function
{
    my ($MDLL, $h_topic) = @_;
    confess q{unimplemented};
}

###########################################################################

sub Array__empty # constant
{
    return $empty_array;
}

sub Array__is_empty # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $empty_array) ? $true : $false;
}

###########################################################################

sub String__empty # constant
{
    return $empty_str;
}

sub String__is_empty # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $empty_str) ? $true : $false;
}

###########################################################################

sub Tuple__nullary # constant
{
    return $nullary_tuple;
}

sub Tuple__is_nullary # function
{
    my ($MDLL, $h_topic) = @_;
    return (refaddr $h_topic == refaddr $nullary_tuple) ? $true : $false;
}

###########################################################################

sub Capsule__select_Capsule # function
{
    my ($MDLL, $topic) = @_;
    my ($h_type, $h_attrs) = @{$topic};
    return $MDLL->_select_Capsule( $h_type, $h_attrs );
}

sub _select_Capsule
{
    my ($MDLL, $h_type, $h_attrs) = @_;
    # Expect $h_type to be an SC_Identifier and $h_attrs to be a Tuple.
    confess q{SC_Identifier isn't of the "identity" subtype}
        if $$h_type->{$VSA_SUBTYPE} ne $IDENT_ST_IDENTITY;
    return _new_v( {
        $VSA_S_KIND => $S_KIND_CPSL,
        $VSA_IDENT  => $$h_type->{$VSA_IDENT},
        $VSA_TUPLE  => $$h_attrs->{$VSA_TUPLE},
    } );
}

sub Capsule__Capsule_type # function
{
    my ($MDLL, $h_topic) = @_;
    # Expect $h to be a Capsule.
    return _new_v( {
        $VSA_S_KIND => $S_KIND_IDENT,
        $VSA_IDENT  => $$h_topic->{$VSA_IDENT},
    } );
}

sub Capsule__attrs # function
{
    my ($MDLL, $h_topic) = @_;
    # Expect $h to be a Capsule.
    return _new_v( {
        $VSA_S_KIND => $S_KIND_TUPLE,
        $VSA_TUPLE  => $$h_topic->{$VSA_TUPLE},
    } );
}

###########################################################################

sub Cast__Tuple__to_SC_Identifier # function
{
    my ($MDLL, $h_topic) = @_;
    # Expect $h to be a Tuple of 4 attributes.
    my $hv = $$h_topic->{$VSA_TUPLE};
    return $MDLL->v_SC_Identifier( [
        [map { $$_->{$VSA_SCALAR} }
            @{${$hv->{pkg_name_base}}->{$VSA_ARRAY}}],
        [map { $$_->{$VSA_SCALAR} }
            @{${$hv->{pkg_name_ext}}->{$VSA_ARRAY}}],
        $MDLL->v_Integer_as_SV( $hv->{rel_starts_n_lev_up} ),
        [map { $$_->{$VSA_SCALAR} }
            @{${$hv->{path_beneath_pkg}}->{$VSA_ARRAY}}],
    ] );
}

sub Cast__SC_Identifier__to_Tuple # function
{
    my ($MDLL, $h_topic) = @_;
    # Expect $h to be an SC_Identifier.
    my $av = $$h_topic->{$VSA_IDENT};
    return $MDLL->v_Tuple( {
        pkg_name_base
            => $MDLL->v_Array( [map { $MDLL->v_String($_) } @{$av->[0]}] ),
        pkg_name_ext
            => $MDLL->v_Array( [map { $MDLL->v_String($_) } @{$av->[1]}] ),
        rel_starts_n_lev_up => $MDLL->v_Integer( $av->[2] ),
        path_beneath_pkg
            => $MDLL->v_Array( [map { $MDLL->v_String($_) } @{$av->[3]}] ),
    } );
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
Native Perl 5 implementation of Muldis_D low level types and routines

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

Copyright © 2011-2015, Muldis Data Systems, Inc.

See the LICENSE AND COPYRIGHT of L<Muldis::D::RefEng> for details.

=cut
