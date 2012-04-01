use 5.008003;
use utf8;
use strict;
use warnings FATAL => 'all';

###########################################################################
###########################################################################

my $MIN_TOKEN_FRAG_LEN =  10;  # in characters
my $DEF_TOKEN_FRAG_LEN = 100;  # in characters

###########################################################################
###########################################################################

{ package Muldis::D::RefEng::StreamLexer; # class
    our $VERSION = '0.000000';

    # ATTRIBUTE LIST:
        # char_stream : A filehandle open for reading, we pull chars from.
        # lookahead : Str : Any extra chars we pulled from char_stream
            # for lookahead that aren't part of the current token/fragment.
        # max_frag_len : Int : Max length for an output token or fragment.
        # is_mid_token : Bool : True iff we have yet to output the last
            # fragment of a token that we already output a fragment of.

    use autodie;
    use Carp;
    use Scalar::Util 'openhandle';

###########################################################################

sub new
{
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($char_stream, $max_frag_len)
        = @{$args}{'char_stream', 'max_frag_len'};

    confess ((caller(0))[3]).q{(): Bad :$char_stream arg;}
            . q{ it must be an open filehandle.}
        if not (ref $char_stream eq 'GLOB' and openhandle $char_stream);
        # We can add support for IO::Handle and other input kinds later.

    if (!defined $max_frag_len)
    {
        $max_frag_len = $DEF_TOKEN_FRAG_LEN;
    }
    else
    {
        confess ((caller(0))[3]).q{(): Bad :$max_frag_len arg; it must be}
                . qq{ undefined or an integer >= $MIN_TOKEN_FRAG_LEN.}
            if not ($max_frag_len =~ m/^[0-9]+$/
                and $max_frag_len >= $MIN_TOKEN_FRAG_LEN);
    }

    binmode $char_stream, ':encoding(UTF-8)';
        # We can add support for non-Unicode(UTF-8)/ASCII input later.
        # If any input isn't valid UTF-8, we expect that the above has
        # been silently replaced with sentinel chars (codepoint 0xFFFD).

    $self->{char_stream}  = $char_stream;
    $self->{lookahead}    = q{};
    $self->{max_frag_len} = $max_frag_len;
    $self->{is_mid_token} = 0;

    return $self;
}

###########################################################################

sub pull_token_or_fragment
{
    my ($self) = @_;
    my $payload = $self->{is_mid_token} ? $self->_continue_token()
        : $self->_start_token();
    return [$self->{is_mid_token}, $payload];
        # Result's first part is a flag that is True iff our caller should
        # expect more fragments of the token in the result, and it is False
        # iff our caller now has all the fragments, or the token is whole.
}

###########################################################################

sub _pull_char
{
    return $_[0]->_pull_n_chars(1);
}

sub _pull_n_chars
{
    my ($self, $cnt) = @_;
    # We expect $cnt is >= 1.
    if ($cnt <= length $self->{lookahead})
    {
        return substr $self->{lookahead}, 0, $cnt, q{};
    }
    else
    {
        my $lookahead = $self->{lookahead};
        $self->{lookahead} = q{};
        read $self->{char_stream}, my $buffer, ($cnt - length $lookahead);
        return $lookahead . $buffer;
    }
}

###########################################################################

} # class Muldis::D::RefEng::StreamLexer

###########################################################################
###########################################################################

1; # Magic true value required at end of a reusable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

Muldis::D::RefEng::StreamLexer -
Process streaming Muldis D source code from characters to tokens

=head1 SYNTAX

    use Muldis::D::RefEng::StreamLexer;

    my $stream_lexer = Muldis::D::RefEng::StreamLexer->new( \*STDIN, 50 );

    while (my $token = $stream_lexer->pull_token_or_fragment())
    {
        my ($is_mid_token, $token_chars) = @{$token};
    }

=head1 DESCRIPTION

Muldis::D::RefEng::StreamLexer provides a stream processor that takes a
stream of Perl characters as input and then outputs a stream of Muldis D
source code syntactical tokens or fragments thereof.

The input stream must be a regular Perl filehandle that is open for
reading, and the data must be valid ASCII or Unicode UTF-8; that is, this
module will apply a binmode of ":encoding(UTF-8)" before reading from it.
In the future, this module will be updated to support other kinds of input.

This module just splits up Muldis D source code strings into substrings,
and it does not add or remove or substitute any characters; if the returned
substrings were simply printed to an output filehandle, the output stream
should be identical to the input.

This module is used internally by L<Muldis::D::RefEng>, such that it serves
as a helper for L<Muldis::D::RefEng::StreamParser>, which assigns more
semantic metadata to the tokens and normalizes them.

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2012, Muldis Data Systems, Inc.

=cut
