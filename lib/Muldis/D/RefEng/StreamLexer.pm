use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use Muldis::D::RefEng::StreamDecoder 0.000000;

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
            # effectively look ahead into the source stream.
            # We sometimes use char_stream_buf as active working memory.
        # is_mid_singquot_str : Bool : 
        # is_mid_doubquot_str : Bool : 
        # is_mid_backtick_str : Bool : 
        # is_mid_slashtar_str : Bool : 
        # is_mid_token : Bool : True iff we have yet to output the last
            # fragment of a token that we already output a fragment of.

    use autodie qw(:all);
    use Carp;

###########################################################################

sub new
{
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($char_stream, $max_frag_len)
        = @{$args}{'char_stream', 'max_frag_len'};

    confess ((caller(0))[3]).q{(): Bad :$char_stream arg;}
            . q{ it must be an open filehandle.}
        if not (ref $char_stream eq 'GLOB' and xxx $char_stream);
        # We can add support for IO::Handle and other input kinds later.

    if (!defined $max_frag_len)
    {
        $max_frag_len = $DEF_TOKEN_FRAG_LEN;
    }
    else
    {
        confess ((caller(0))[3]).q{(): Bad :$max_frag_len arg; it must be}
                . qq{ undefined or an integer >= $MIN_TOKEN_FRAG_LEN.}
            if ref $max_frag_len or not ($max_frag_len =~ m/^[0-9]+$/
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

sub max_frag_len
{
    return $_[0]->{max_frag_len};
}

sub update_max_frag_len
{
    my ($self) = @_;
    my ($max_frag_len) = @{$args}{'max_frag_len'};

    if (!defined $max_frag_len)
    {
        $max_frag_len = $DEF_TOKEN_FRAG_LEN;
    }
    else
    {
        confess ((caller(0))[3]).q{(): Bad :$max_frag_len arg; it must be}
                . qq{ undefined or an integer >= $MIN_TOKEN_FRAG_LEN.}
            if ref $max_frag_len or not ($max_frag_len =~ m/^[0-9]+$/
                and $max_frag_len >= $MIN_TOKEN_FRAG_LEN);
    }

    $self->{max_frag_len} = $max_frag_len;
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
        char_stream => Muldis::D::RefEng::StreamDecoder->new({
        raw_stream => \*STDIN }), max_frag_len => 150 });

    while (my $token = $stream_lexer->pull_token_or_fragment())
    {
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

Each returned token or fragment is simply a Perl character string, and no
metadata is returned with it.  Since returned tokens are rather
fine-grained (for example, each delimiter of a string literal of the input
is its own token), the user of this module should be able to tell easily
enough what kind each token is, and whether or not it is complete or a
fragment, just from said character strings themselves.

=head1 UNDERSTANDING TOKENS

Broadly speaking, every token or fragment that this module outputs will
arise from 2 disjoint and complementary contexts within the input, one of
which is anywhere outside all quoted strings, and the complement is
anywhere inside any quoted string; quoted strings don't overlap or nest, or
if they appear to, any inner ones are just regular string contents.

There are 2 main forms of quoted strings.  The first form is code comments,
whose opening delimiter is C</*> and whose closing delimiter is C<*/>, and
any occurrence of C<*/> meant to be part of the string content must be
C<\>-prefixed.  The second form is non-comments, which has a sub-form for
each of the 3 delimiter characters {C<'>,C<">,C<`>}, where for each
sub-form both the opening and closing delimiter must be the same, and any
occurrence of that delimiter character meant to be part of the string
content must be C<\>-prefixed.

This module will always return each occurrence of a string delimiter that
causes the transition to or from quoted string context as its own whole
token (never a fragment), and so every token except one of these is either
wholly inside or outside quoted string context, as is every fragment.

=head2 Outside String Context

Within normal input code, outside string context, this module will always
transition between tokens where the input character string transitions
between any 2 of the following character groups:

=over

=item B<Alphanumeric>

The ASCII letters, digits, and underscore; that is:
C<{'a'..'z','A'..'Z','0'..'9','_'}>

=item B<Standalone>

The ASCII backslash, comma, parenthesis, brackets, braces; that is:
C<{'\',',','(',')','[',']','{','}'}>.  Moreover, as a special exception,
any runs of these characters are also split into tokens as well, so any
occurrence of these will produce single-character tokens (never fragments).

=item B<Symbolic>

The printable ASCII characters not otherwise mentioned, including the
comment string delimiter characters when not in that exact sequence; that
is: C<{'!','#','$','%','&','*','+','-','.','/',':',
'<','=','>','?','@','^','|','~'}>.  Moreover, as a special exception, a run
of these characters will be split further into tokens if a C<:> is present;
specifically, any C<::=> will be its own token (never fragment), or
otherwise any C<:=> will be its own, or else any C<:> will be its own.

=item B<Whitespace>

The ASCII space, tab, linebreak, carriage return; that is: C<{'
','\t','\n','\r'}> aka C<{0x20,0x9,0xA,0xD}>.  Moreover, as a special
exception, a run of these characters will be split further into tokens if
any linebreaks are present, such that any logical line separator will be
its own token (never fragment); specifically, any C<[0xD,0xA]> pair will be
its own token, or otherwise any C<0xA> or C<0xD> will be its own.

=item B<Illegal>

All ASCII control characters besides those named in B<Whitespace> may not
appear literally in the input stream; that is: C<{0x0..0x8,0xB..0xC,
0xE..0x1F,0x7F}>.  If these are found, runs of such will be returned as
their own tokens or fragments, and the user code should complain about it.

=back

The above applies specifically when the input code declares itself
specifically to have the ASCII character repertoire.  When the input code
declares itself to be Unicode or some other repertoire instead, then these
sets of characters are extended with a useful whitelist: B<Alphanumeric>,
B<Symbolic>, B<Whitespace>; any other characters from the wider repertoire
go into B<Illegal>.  To be more specific, probably the only B<Whitespace>
addition would be the one or so line separator characters added by Unicode,
while C<Symbolic> would likely gain the most additions, at least several
dozen mathematical/etc operator symbols; B<Alphanumeric> may gain the alpha
characters of several other scripts, if it is useful to do so, but
certainly not any numerics of other scripts.

=head2 Inside String Context

Inside string context, this module will always transition between tokens
where the input has any of the following characters:

=over

=item B<Line Separator>

Any logical line separator will be its own token (never fragment);
specifically, any C<[0xD,0xA]> pair will be its own token, or otherwise any
C<0xA> or C<0xD> will be its own.

=item B<Escape Sequence>

The ASCII backslash (C<\>) where found will be returned as its own token.
Moreover, the single character immediately following it will also be
returned as its own token, except where a B<Line Saparator> immediately
follows the backslash.  Moreover, if the aforementioned single character is
C<c> then a subsequent short sequence of characters identifying the
codepoint of another character will also be returned as its own token
(possibly fragmented in degenerate cases).

=item B<Illegal>

All control characters besides a short whitelist consisting of space, tab
and line separators may not appear literally in the input stream.  If these
are found, runs of such will be returned as their own tokens or fragments,
and the user code should complain about it.  Logically including any such
characters in strings may only be done using B<Escape Sequence>.

=back

When the input code declares itself to be Unicode or some other repertoire
instead, then generally within string context any printable character may
appear literally in the input stream, while generally any non-printable or
control character may not; there would be a white-list for the
non-printable characters allowed, and any others may only be specified
using B<Escape Sequence>.

=head1 INTERFACE

The interface of Muldis::D::RefEng::StreamLexer is entirely
object-oriented; you use it by creating objects of the
Muldis::D::RefEng::StreamLexer class and then invoking methods on those
objects.  All of their attributes are private, so you must use accessor
methods.

The usual way that Muldis::D::RefEng::StreamLexer indicates a failure is to
throw an exception; most often this is due to invalid input.  If an invoked
routine simply returns, you can assume that it has succeeded, even if the
return value is undefined.

...

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
older supported Perl 5 versions: L<autodie>, L<Carp>.

It also requires these Perl 5 packages that are in the current
distribution:
L<Muldis::D::RefEng::StreamDecoder-ver(0.0.0..*)|Muldis::D::RefEng::StreamDecoder>.

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2013, Muldis Data Systems, Inc.

=cut
