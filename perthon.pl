# perthon.pl
#
# Perthon -- The Python to Perl translator
# David Manura, 2003-11

use strict;
use lib 'lib';
use Getopt::Long;
use Perthon::Perthon;

&main();

sub main
{
    my $help = 0;
    my $output = undef;

    my $result = GetOptions(
        "output=s" => \$output, #FIX:TODO
        "help" => \$help,
    );


    if(@ARGV == 0 || $help) {
        &print_usage();
        exit(1);
    }

    my $filename = $ARGV[0];


    open(INFILE, $filename)
        || die "$! $filename\n";
    my $text = join('', <INFILE>);
    close(INFILE);

    # run sample (DEBUG)
    print &translate($text);
}

sub print_usage
{
    print STDERR "usage: perthon <filename.py>\n"
}
