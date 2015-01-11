use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

###########################################################################
###########################################################################

{ package Muldis::D::RefEng::StreamDecoder; # class
    BEGIN {
        our $VERSION = '0.000000';
        $VERSION = eval $VERSION;
    }

    # ATTRIBUTE LIST:
        # raw_stream : A filehandle open for reading, we pull input from.
        # raw_stream_buf : Str (ref) : Any input we pulled from raw_stream
            # that we haven't yet normalized for output; this is
            # conceptually un-pulled but we're using it to
            # to effectively look ahead into the source stream.
        # out_stream_buf : Str (ref) : Any chars that have been normalized
            # and'r ready for output.  The pull_char method will first take
            # the first codepoint from out_stream_buf if there are any, and
            # only when out_stream_buf is empty will raw_stream/buf draw.

    use autodie qw(:all);
    use Carp;
    use Scalar::Util 'openhandle';

    use Unicode::Normalize 1.16;

###########################################################################

sub new
{
    my ($class, $args) = @_;
    my $self = bless {}, $class;
    my ($raw_stream) = @{$args}{'raw_stream'};

    confess ((caller(0))[3]).q{(): Bad :$raw_stream arg;}
            . q{ it must be an open filehandle.}
        if not (ref $raw_stream eq 'GLOB' and openhandle $raw_stream);
        # We can add support for IO::Handle and other input kinds later.

    binmode $raw_stream, ':encoding(UTF-8)';
        # We can add support for non-Unicode(UTF-8)/ASCII input later.
        # If any input isn't valid UTF-8, we expect that the above has
        # been silently replaced with sentinel chars (codepoint 0xFFFD).

    $self->{raw_stream}     = $raw_stream;
    $self->{raw_stream_buf} = do { my $rsb = q{}; \$rsb; };
    $self->{out_stream_buf} = do { my $osb = q{}; \$osb; };

    return $self;
}

###########################################################################

# MAYBE CHANGE THIS API TO ADD OFFSETS FOR PEEKING?

sub pull_char
{
    my ($self) = @_;
    $self->_extend_out_stream_buf_to_count( 1 );
    # Slice 1 char from start of out_stream_buf and return sliced.
    return substr ${$self->{out_stream_buf}}, 0, 1, q{};
}

sub pull_n_chars
{
    my ($self, $args) = @_;
    my ($count) = @{$args}{'count'};

    confess ((caller(0))[3]).q{(): Bad :$count arg; it must be}
            . qq{ an integer >= 1.}
        if !defined $count or ref $count
            or not ($count =~ m/^[0-9]+$/ and $count >= 1);

    $self->_extend_out_stream_buf_to_count( $count );
    # Slice $count chars from start of out_stream_buf and return sliced.
    return substr ${$self->{out_stream_buf}}, 0, $count, q{};
}

sub peek_char
{
    my ($self) = @_;
    $self->_extend_out_stream_buf_to_count( 1 );
    # Copy 1 char from start of out_stream_buf and return that.
    return substr ${$self->{out_stream_buf}}, 0, 1;
}

sub peek_n_chars
{
    my ($self, $args) = @_;
    my ($count) = @{$args}{'count'};

    confess ((caller(0))[3]).q{(): Bad :$count arg; it must be}
            . qq{ an integer >= 1.}
        if !defined $count or ref $count
            or not ($count =~ m/^[0-9]+$/ and $count >= 1);

    $self->_extend_out_stream_buf_to_count( $count );
    # Copy $count chars from start of out_stream_buf and return that.
    return substr ${$self->{out_stream_buf}}, 0, $count;
}

###########################################################################

# This routine makes sure that raw_stream_buf contains at least $count raw
# chars; it should only return short of that if raw_stream is exhausted.

sub _extend_raw_stream_buf_to_count
{
    my ($self, $count) = @_;
    # We expect $count is >= 1.
    my $rsb_ref = $self->{raw_stream_buf};
    if (length $$rsb_ref < $count)
    {
        read $self->{raw_stream}, $$rsb_ref,
            $count - length $$rsb_ref, length $$rsb_ref;
    }
}

###########################################################################

# This routine makes sure that out_stream_buf contains at least $count
# normalized chars; it should only return short if raw_stream is exhausted.

sub _extend_out_stream_buf_to_count
{
    my ($self, $count) = @_;
    # We expect $cnt is >= 1.
    my $osb_ref = $self->{out_stream_buf};
    if (length $$osb_ref < $count)
    {
        # See how many more normalized chars we need to procure.
        my $osb_shortfall = $count - length $$osb_ref;

        my $rsb_ref = $self->{raw_stream_buf};
        if (length $$rsb_ref < 1 + $osb_shortfall)
        {
            $self->_extend_raw_stream_buf_to_count( 1 + $osb_shortfall );
        }
        # At this point either raw_stream_buf should contain at least 1
        # more raw character than the count of normalized characters we are
        # short by, or raw_stream is exhausted.

        # Now, since normalization to NFD or NFKD will either leave alone
        # or split raw_stream_buf characters, we can guarantee that not
        # more than $osb_shortfall chars will be moved from raw_stream_buf
        # to out_stream_buf, except when the raw char immediately following
        # the last raw char that would have been moved is a combining char,
        # in which case any single or run of combining chars starting there
        # would also be moved, to ensure that all codepoint sequences in
        # out_stream_buf which are mutually combinable are in canonical
        # order; this might require further extending raw_stream_buf if so.

        # TODO, TESTS FOR CHARACTER PROPERTIES, DOING APPROP NORMALIZE ETC
        # SEE ALSO:
        # UAX 15 - 13.1 Buffering with Unicode Normalization
        # UAX 15 - 9 Detecting Normalization Forms
        # QUOTH:
#         That is, as decompositions are appended to the buffer,
#         periodically the end of the buffer will be reached. At that time,
#         the characters in the buffer up to but not including the last
#         character with the property value Quick_Check=Yes (QC=Y) must be
#         canonically ordered (and if NFC and NFKC are being generated, must
#         also be composed), and only then flushed.

        ...;

    }
}

###########################################################################

} # class Muldis::D::RefEng::StreamDecoder

###########################################################################
###########################################################################

1; # Magic true value required at end of a reusable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

Muldis::D::RefEng::StreamDecoder -
Process streaming Muldis D source code from raw form to characters

=head1 SYNTAX

    use Muldis::D::RefEng::StreamDecoder;

    my $stream_decoder = Muldis::D::RefEng::StreamDecoder->new({
        raw_stream => \*STDIN });

    while (my $char = $stream_decoder->pull_char())
    {
        # And do work with $char.
    }

=head1 DESCRIPTION

Muldis::D::RefEng::StreamDecoder provides a stream processor that takes a
stream of possibly encoded characters as input and then outputs a stream of
Perl native/internal and possibly normalized characters.

This module is used internally by L<Muldis::D::RefEng>, such that it serves
as a helper for L<Muldis::D::RefEng::StreamLexer>, which groups the
characters into token strings.

The input stream must be a regular Perl filehandle that is open for
reading, and the data must be valid ASCII or Unicode UTF-8; that is, this
module will apply a binmode of ":encoding(UTF-8)" before reading from it.
In the future, this module will be updated to support other kinds of input,
meaning either non-UTF-8 filehandles or Perl string values.

This module's primary purpose is to cause the interpretation of two
arbitrary Muldis D source code strings to be identical when they differ
only by encoding or Unicode normal form.  This module outputs a character
stream where each character is a Perl native character string consisting of
a single Unicode (superset) codepoint that is noncomposed, no matter the
encoding of the input.  This module's output is always Unicode canonical
decomposed normal form (NFD).  If the returned characters were simply
printed to an output filehandle in the same order, the output stream/file
should be identical to the input, for all practical intents and purposes.

As an optional feature, this module can be configured to also fold its
input into subset character repertoire, by way of Unicode compatibility
decomposed normal form (NFKD), which may be useful for security.  Muldis D
source code is expected to declare whether it expects this compatibility
folding to occur, and if so then the user of this module should check for
this and change the configuration of this module as appropriate.

This module always performs at least Unicode canonical normalization on its
input and so if you want to specify character data that is not in this
normal form then you can not do it in Muldis D source code with literal
codepoints but rather you must use escape sequences.

This module can optionally buffer the input stream, so that after its user
gets some output, it can choose to start over and have this module
re-process the same input into possibly different output.  For example,
there might be initial runs just to scan for instructions on how to
interpret the entire stream, such as an ambiguity-resolving encoding
declaration or a request for the compatibility folding.  But even absent an
explicit buffering request, a few input characters may be buffered by this
module in order to work, and so external reading of the underlying input
filehandle is not advised as it may miss some characters.

=head1 INTERFACE

The interface of Muldis::D::RefEng::StreamDecoder is entirely
object-oriented; you use it by creating objects of the
Muldis::D::RefEng::StreamDecoder class and then invoking methods on those
objects.  All of their attributes are private, so you must use accessor
methods.

The usual way that Muldis::D::RefEng::StreamDecoder indicates a failure is to
throw an exception; most often this is due to invalid input.  If an invoked
routine simply returns, you can assume that it has succeeded, even if the
return value is undefined.

=head1 Constructor Submethods

This is currently the only routine declared by
Muldis::D::RefEng::StreamDecoder that you invoke off of the class name;
currently you invoke all other routines off of a
Muldis::D::RefEng::StreamDecoder object.

=head2 new

C<submethod new of Muldis::D::RefEng::StreamDecoder (FileHandle :$raw_stream)>

This constructor submethod creates and returns a new
C<Muldis::D::RefEng::StreamDecoder> object, which is a wrapper and buffer
for the (open and ready for reading) regular Perl filehandle given, to
provide that object's input stream, in its C<$raw_stream> argument.

=head1 Object Methods

=head2 pull_char

C<method pull_char of Str ($self:)>

This method returns the next Perl character / noncomposed Unicode codepoint
of the object's output stream, also advancing that stream so that the next
invocation of C<pull_char> or C<peek_char> will return the subsequent
character.

=head2 pull_n_chars

C<method pull_n_chars of Str ($self: PInt :$count)>

This method is a generalization of C<pull_char> that returns in a single
string the next C<$count> characters instead of just the next 1.

=head2 peek_char

C<method peek_char of Str ($self:)>

This method is like C<pull_char> but that it does I<not> advance the output
stream, so the next invocation of C<pull_char> or C<peek_char> will return
the I<same> character.

=head2 peek_n_chars

C<method peek_n_chars of Str ($self: PInt :$count)>

This method is a generalization of C<peek_char> that returns in a single
string the next C<$count> characters instead of just the next 1.

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1, and
recommends one that is at least 5.16.2.

It also requires these Perl 5 packages that are bundled with at least the
latest production version of Perl 5, or are otherwise available on CPAN for
older supported Perl 5 versions: L<autodie>, L<Carp>, L<Scalar::Util>.

It also requires these Perl 5 packages that are on CPAN:
L<Unicode::Normalize-ver(1.16..*)|Unicode::Normalize>.

=head1 BUGS AND LIMITATIONS

Muldis::D::RefEng::StreamDecoder depends on L<Unicode::Normalize> for its
exact behavior concerning the specifics of Unicode normal forms, and that
module in turn refers to the Perl core's Unicode database in the directory
/lib/unicore (or formerly /lib/unicode), and so the Unicode version of
normalization implemented by this module depends on your Perl's version.
This is in contrast to the latest version of the Muldis D language
specification which says that Unicode version 6.2.0 defines the behavior.
See L<Unicode::Normalize> main section "CAVEATS" for more details on this.

=head1 AUTHOR

Darren Duncan - darren@DarrenDuncan.net

=head1 LICENSE AND COPYRIGHT

Copyright © 2011-2013, Muldis Data Systems, Inc.

=cut
