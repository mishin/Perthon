# note how Perl has different scoping rules for class/package names
# than Python (see class.py).  In Perl, classes are more global and static.

use strict;
my $z=new Test("z")->{x};
print $z;
if(0) {
    {
        package Test;
        sub new {
            print "Test::new $_[1]\n";
            return bless {x=>2};
        }
    };
    my $x=new Test("x");
}
my $y=new Test("y");

