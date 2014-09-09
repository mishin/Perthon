# see Python Reference Manual, Section 2.1. Line structure
#   http://www.python.org/doc/current/ref/line-structure.html

use strict;
use Test::More 'no_plan';
use Perthon::Perthon;

is(&translate(<<'TEXT'
def test():

    #test
  
  \
  x=1
\
    x=2
TEXT
), qq(sub test {\n    \$x = 1;\n}\n\$x = 2;\n),
"syntax: obscure indentation rules");

is(&translate(<<'TEXT'
def test():

  x=1

#ok
  y=2
TEXT
), qq(sub test {\n    \$x = 1;\n    \$y = 2;\n}\n),
"syntax: multiple physical newlines");

# FIX: remove for improvement
is(&translate(<<'TEXT'
x = '''123
234\n'''
y = """234
345\n"""
TEXT
), qq(\$x = qq(123\n234\\n);\n\$y = qq(234\n345\\n);\n),
"syntax: long string");

is(&translate(<<'TEXT'
x = "123\
456"
y = '''123\
456'''
z = """123\
456"""
TEXT
), qq(\$x = "123456";\n\$y = qq(123456);\n\$z = qq(123456);\n),
"syntax: continued string");

like(&translate(<<'TEXT'
x = \  #test
2
TEXT
), qr/[EFIX]/,
"syntax: comment after explicit continuation (bad)");

is(&translate(<<'TEXT'
x = "123"  #test\
TEXT
), qq(\$x = "123";\n),
"syntax: explicit continuation ignored after comment");

like(&translate(<<'TEXT'
x\
1=2
TEXT
), qr/[EFIX]/,
"syntax: explicit continuation inside token (bad)");

is(&translate(<<'TEXT'
x=\
5
TEXT
), qq(\$x = 5;\n),
"syntax: explicit continuation");

# FIX: "&" char on calls
is(&translate(<<'TEXT'
x = y[ #test
1]
y = abs(3 #test2
)
z = abs(

# ok
2
)
TEXT
), qq(\$x = \$y->[1];\n\$y = \&abs(3);\n\$z = \&abs(2);\n),
"syntax: implicit continuation + comment");


