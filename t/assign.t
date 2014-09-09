use strict;
use Test::More 'no_plan';
use Perthon::Perthon;

is(&translate(<<'TEXT'), qq(\$x = [1, 2];\n), "assign: x=1,2");
x = 1,2
TEXT
