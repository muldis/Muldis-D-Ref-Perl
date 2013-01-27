use 5.008001;
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
        # max_frag_len : Int : Max length for an output token or fragment.
        # char_stream : A filehandle open for reading, we pull chars from.
        # char_stream_buf : Str : Any chars we pulled from char_stream that
            # weren't yet output as a token or fragment; these are
            # conceptually un-pulled chars but we're using them to
            # to effectively look ahead into the source stream.
            # We sometimes use char_stream_buf as active working memory.
        # is_mid_token : Bool : True iff we have yet to output the last
            # fragment of a token that we already output a fragment of.

    use autodie qw(:all);
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

    $self->{max_frag_len}    = $max_frag_len;
    $self->{char_stream}     = $char_stream;
    $self->{char_stream_buf} = do { my $csb = q{}; \$csb; };
    $self->{is_mid_token}    = 0;

    return $self;
}

###########################################################################

sub pull_token_or_fragment
{
    my ($self) = @_;
    my $payload = $self->{is_mid_token} ? $self->_continue_token()
        : $self->_start_token();
    return {
        chars        => $payload,
        is_mid_token => $self->{is_mid_token},
    };
}

###########################################################################

sub _pull_n_chars_append_into_buffer
{
    my ($self, $cnt) = @_;
    # We expect $cnt is >= 1.
    my $csb_ref = $self->{char_stream_buf};
    read $self->{char_stream}, $$csb_ref, $cnt, length $$csb_ref;
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

    my $stream_lexer = Muldis::D::RefEng::StreamLexer->new({
        char_stream => \*STDIN, max_frag_len => 150 });

    while (my $token = $stream_lexer->pull_token_or_fragment())
    {
        my ($chars, $is_mid_token) = @{$token}{'chars', 'is_mid_token'};
        # And do work with $token.
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
substrings were simply printed to an output filehandle in the same order,
the output stream/file should be identical to the input.

This module is used internally by L<Muldis::D::RefEng>, such that it serves
as a helper for L<Muldis::D::RefEng::StreamParser>, which assigns more
semantic metadata to the tokens and normalizes them.

This module generally returns whole tokens at once, as typically each one
is quite short (such as for an operator name or numeric literal), but
sometimes it returns tokens in fragments instead, such as because the whole
token would be quite long (such as for some Text or Blob literals) and we
wish to preserve working memory, either for performance or for security,
or protection against malformed input (such as an unterminated string).

Each returned token or fragment carries several pieces of metadata:

=over *

=item

C<is_mid_token> - A Boolean,
that is False iff what was returned is either a whole
token or the last piece of a fragmented one, and True iff what was returned
is the first or middle fragment of a token being returned not all at once.

=back

The C<max_frag_len> optional configuration parameter lets you tune the
maximum size of each fragment in characters to wherever it might be optimal
for performance or debugging; it defaults to 100 and must be at least 10 if
given.  By default, longer tokens will be split into fragments of the
configured size, but that as a special case, if the token contains any
line-ending characters such as C<\n> or C<\r> then it will be split
immediately following those, so that this module's user can more easily
report debugging information in terms of source code line numbers.

And so, if C<max_frag_len> is set to be larger than all source code lines,
then the only fragmentation that will occur is when any delimited strings
or comments wrap around to multiple source lines; hence the default of 100.

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1, and
recommends one that is at least 5.16.2.

It also requires these Perl 5 packages that are bundled with at least the
latest production version of Perl 5, or are otherwise available on CPAN for
older supported Perl 5 versions: L<autodie>, L<Carp>, L<Scalar::Util>.

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2013, Muldis Data Systems, Inc.

=cut
