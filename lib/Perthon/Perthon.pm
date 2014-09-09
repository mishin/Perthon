# Perthon.pm
# The Perthon module
# David Manura, 2003-11-25, created

use strict;
use warnings;
use Perthon::PerthonImpl;
use Text::Tabs;


my $pe = new Perthon::PerthonImpl;


# Parse::RecDescent error options
$::RD_ERRORS = 1;     # unless undefined, report fatal errors
$::RD_WARN = 1;       # unless undefined, also report non-fatal problems
$::RD_HINT = 1;       # if defined, also suggestion remedies
#$::RD_TRACE = 1;      # if defined, also trace parsers' behaviour


# even though Parse::RecDescent does most of the lexing,
# we'll manually take care of the interpretation of
# Pythonic spacing.  This function will insert the
# INDENT and DEDENT tokens
#
# params:
#   s : string - input
# return : string - output
sub lex_preprocess
{
    my($s) = @_;

    # remove these chars, as I don't believe they are useful
    # but would only complicate the parsing
    $s =~ tr/\r\f//d;

    $s = expand($s); # untabify

    $s .= "\n"; # ensure eol (needed still?)

    # remove comments
    # IMPROVE: preserve comments?   FIX:not general
    #$s =~ s/\#.*$//gm;

    # ref. http://www.python.org/doc/current/ref/indentation.html
    my @stack;
    push @stack, 0;

    my $shortstring_re = qr/
        \'[^\\\n\']*(\\.[^\\\n\']*)*\'
        | \"[^\\\n\"]*(\\.[^\\\n\"]*)*\"
    /x;
    my $longstring_re = qr/
        \'\'\'[^\\]*?(\\.[^\\]*?)*?\'\'\'
        | \"\"\"[^\\]*?(\\.[^\\]*?)*?\"\"\"
    /x;

    # preprocess lines
    #Q:efficiency?
    use re 'eval'; # prevents regex error: interpolation + embedded code
    my $nest = 0;  # implicit line continuation inside (), {}, and []
    $s =~ s{
        # whitespace for determining indent level
        ^([ ]*)
        # rest
        ( (?:$longstring_re
             | $shortstring_re
             | \#.*        # comments
             | \\\n        # explicit line cont.
             | [\(\[\{] (?{$nest++;})
             | [\)\]\}] (?{$nest--})
             # handles implicit line cont.
             | (??{$nest > 0 ? qr/.|\n/ : qr/[^\n]/ })
          )*
        )
    }{
        my $spaces = length($1);
        #print STDERR "[$spaces]\n";
        my $line_rest = $2;
        my $new_line = '';

        # generate INDENT/DEDENTs
        # note: only some lines count towards indentation
        if($line_rest =~ /^(\\|[^\#])/) {
            if($spaces > $stack[@stack-1]) {
                $new_line .= "!!INDENT ";
                push @stack, $spaces;
            }
            else {
                while($spaces < $stack[@stack-1]) {
                    $new_line .= "!!DEDENT ";
                    pop @stack;
                }
                if($spaces != $stack[@stack-1]) {
                    die "[EX] inconsistent dedent";
                }
            }
        }
        $new_line .= $line_rest;

        $new_line
    }xgme;
    # cleanup remaining DEDENTS
    while(pop(@stack) > 0) {
        $s .= " !!DEDENT ";
    }

    return $s;
}

sub translate
{
    my($python) = @_;

    my $s = &lex_preprocess($python);
    #die "lexed:$s";
    $pe->file_input($s);

}



1
