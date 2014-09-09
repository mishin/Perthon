# grammar.pl
# Parse::RecDescent grammar for Python-to-Perl translator
#
# Refer to http://www.python.org/doc/current/ref/grammar.txt
# for the left-recursive Python grammar.  Parse::RecDescent
# requires that this grammar be converted to right-recursive.
#
# Note: the use of lookaheads is for better error detection.
#
# Perthon is licensed under the same terms as Perl itself.
# David Manura, 2003-11, created

#-- code initialization
{
    use strict;
    use Data::Dumper;

    # normal skipping
    my $skip_outer = qr/( [ \t] | \\\n )*/x;
    # skipping when inside (), {}, or []
    my $skip_inner = qr/( [ \t\n] | \\\n | \#[^\n]* )*/x;

    sub indent
    {
        my($s) = @_;
        return join '', map "    $_", ($s =~ /.*\n/g);
    }

    sub optional
    {
        my($ele) = @_;
        return @$ele > 0 ? $$ele[0] : undef;
    }
    sub def
    {
        my($o) = @_;
        return defined($o) ? $o : '';
    }
}

#--- not in grammar.txt

#old: NEWLINE : /\r\n?|\n/
# note: assume eol chars in text have been normalized
# note: multiple phyiscal newlines are merged into a single logical
#   newline
NEWLINE : /((?:\#[^\n]*)?\n)+/

end_of_file : /^\Z/

#FIX: make sure these don't conflict with regular code
INDENT : '!!INDENT'
DEDENT : '!!DEDENT'


#--- grammar.txt items

## identifier ::= 
##             (letter|"_") (letter | digit | "_")*

identifier : /[a-zA-Z_][a-zA-Z0-9_]*/
{
    my $perl = $item[1];
    {perl => $perl}
}

## letter ::= 
##              lowercase | uppercase

# unused


## lowercase ::= 
##              "a"..."z"

# unused

## uppercase ::= 
##              "A"..."Z"

# unused

## digit ::= 
##              "0"..."9"

# unused


## stringliteral ::= 
##              [stringprefix](shortstring | longstring)

# note: longstring before shortstring
stringliteral : #FIX: [stringprefix](
                longstring | shortstring # )

## stringprefix ::= 
##              "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR"

#FIX:TODO

## shortstring ::= 
##              "'" shortstringitem* "'"
##               | '"' shortstringitem* '"'

# note: also handle explicit line continuations inside string
#   (normally, this would be the lexer's responsibility)
shortstring : /
      \' (?:[^\\\n\']|\\\n)* ( \\. (?:[^\\\n\']|\\\n)* )* \'
    | \" (?:[^\\\n\"]|\\\n)* ( \\. (?:[^\\\n\"]|\\\n)* )* \"
/x
{
    my $str = $item[1];
    $str =~ s/\\\n//gs; # remove line continuations (since Perl interprets
                        # these differently inside strings
    #Q:how to handle variable interpolations in Perl?
    {perl => $str}
}

## longstring ::= 
##              "'''" longstringitem* "'''"
##                 | '"""' longstringitem* '"""'

# note: also handle explicit line continuations inside string
#   (normally, this would be the lexer's responsibility)
longstring : /
       (\'\'\'|\"\"\") (?:[^\\]|\\\n)*? ( \\. (?:[^\\]|\\\n)*? )*? \1
/x
{
    ($item[1] =~ /^(\'\'\'|\"\"\")(.*)\1$/s)
        || die "[EX] error in compiler";
    my $inner = $2;
    $inner =~ s/\\\n//gs; # remove line continuations (since Perl interprets
                        # these differently inside strings
    #FIX: handle the general case
    #Q:how to handle variable interpolations in Perl?
    my $perl = "qq($inner)";
    {perl => $perl}
}

## shortstringitem ::= 
##              shortstringchar | escapeseq

# unused

## longstringitem ::= 
##              longstringchar | escapeseq

# unused

## shortstringchar ::= 
##              <any ASCII character except "\" or newline or the quote>

# unused


## longstringchar ::= 
##              <any ASCII character except "\">

# unused


## escapeseq ::= 
##              "\" <any ASCII character>

# unused

## longinteger ::= 
##              integer ("l" | "L")

#FIX:TODO  

## integer ::= 
##              decimalinteger | octinteger | hexinteger

integer : decimalinteger # | octinteger | hexinteger

## decimalinteger ::= 
##              nonzerodigit digit* | "0"

  
decimalinteger : /[1-9][0-9]*|0/
{
    {perl => $item[1]}
}

## octinteger ::= 
##              "0" octdigit+

octinteger : /0[0-7]+/
{
    {perl => $item[1]}
}

## hexinteger ::= 
##              "0" ("x" | "X") hexdigit+

hexinteger : /0[xX][0-9a-fA-F]+/
{
    {perl => $item[1]}
}
  

## nonzerodigit ::= 
##             "1"..."9"

# unused

## octdigit ::= 
##              "0"..."7"

# unused

  
## hexdigit ::= 
##              digit | "a"..."f" | "A"..."F"

# unused


## floatnumber ::= 
##              pointfloat | exponentfloat

# simplify possible?
floatnumber : /([0-9]*\.[0-9]+|[0-9]+\.?)[eE][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\./
{
    {perl => $item[1]}
}

## pointfloat ::= 
##              [intpart] fraction | intpart "."

# unused


## exponentfloat ::= 
##              (intpart | pointfloat)
##               exponent

# unused


## intpart ::= 
##              digit+

# unused


## fraction ::= 
##              "." digit+

# unused


## exponent ::= 
##              ("e" | "E") ["+" | "-"] digit+

# unused


#imagnumber : (floatnumber | intpart) ("j" | "J")
#

# FIX:TODO

## atom ::= 
##              identifier | literal | enclosure

atom : identifier
{
    $item[1]->{type} = 'identifier';
    $item[1]
}
| literal
{
    $item[1]
}
| enclosure
{
    $item[1]
}

## enclosure ::= 
##              parenth_form | list_display
##                 | dict_display | string_conversion
 
enclosure : 
#             parenth_form |
              list_display
              | dict_display
                # | string_conversion
#

## literal ::= 
##              stringliteral | integer | longinteger
##                 | floatnumber | imagnumber

literal : 
    # note: float before integer
    floatnumber |
    stringliteral | 
    integer #| longinteger
    # | imagnumber


## parenth_form ::= 
##              "(" [expression_list] ")"

#FIX:TODO
#note: <skip: $skip_inner>

## test ::= 
##              and_test ( "or" and_test )*
##               | lambda_form

#FIX:TODO

## testlist ::= 
##              test ( "," test )* [ "," ]

#FIX:TODO

## list_display ::= 
##              "[" [listmaker] "]"

list_display : "[" <skip: $skip_inner> listmaker(?) "]"
{
    my $listmaker = &optional($item{'listmaker(?)'});
    my $list = defined($listmaker) ? $listmaker : [];
    my $perl = "[" . join(', ', map "$$_{perl}", @$list) . "]";
    {perl => $perl}
}

## listmaker ::= 
##              expression ( list_for
##               | ( "," expression )* [","] )

listmaker : expression ( # FIX: list_for |
              ("," expression)(s?) (",")(?) {$item[1]}
            )
{
    my @exps = ($item[1], @{$item[2]});
    \@exps
}

## list_iter ::= 
##              list_for | list_if

#FIX:TODO

## list_for ::= 
##              "for" expression_list "in" testlist
##               [list_iter]

#FIX:TODO

## list_if ::= 
##              "if" test [list_iter]

#FIX:TODO

## dict_display ::= 
##              "\{" [key_datum_list] "\}"


dict_display_list : (expression ':' expression {[$item[1], $item[3]]})(s /,/)
dict_display : '{' <skip: $skip_inner> ( dict_display_list (',')(?) {$item[1]} )(?) '}'
{
    my $pairs = &optional($item[3]) || [];
    my $perl = '{'
        . join(', ', map "$$_[0]->{perl} => $$_[1]->{perl}", @$pairs) . '}';
    {perl => $perl}
}


## key_datum_list ::= 
##              key_datum ("," key_datum)* [","]

# unused


## key_datum ::= 
##              expression ":" expression

# unused

## string_conversion ::= 
##              "`" expression_list "`"

#FIX:TODO

## primary ::= 
##              atom | attributeref
##               | subscription | slicing | call

# note: conversion from left-recursive to right-recursive
primary : atom (call | attributeref | subscription
    | slicing
    )(s?)
{
    my $ends = $item[2];

    #print STDERR Dumper(\%item);
    my $atom = $item{atom}->{perl};
        
    my $perl = $atom;
    if(&def($item{atom}->{type}) eq 'identifier') {
        $perl = "\$$perl";
    }
    for(my $n=0; $n<@$ends; $n++) {
        my $end = $$ends[$n];
        if(&def($$end{type}) eq 'call') {
            if($n == 0) {
                # this might be either function call or a constructor.
                # since we don't know, will mark to for later correction.
                $perl = "__defer_call_$atom$$end{perl}";
	    }
            else {
                $perl = "$perl->$$end{perl}";
	    }
        }
        elsif(&def($$end{type}) eq 'attributeref') {
            $perl = "[$perl->$$end{perl}]";
	}
        elsif(&def($$end{type}) eq 'subscription') {
            $perl = "$perl->$$end{perl}";
	}
        elsif(&def($$end{type}) eq 'slicing') {
            $perl = "[\@{$perl}$$end{perl}]";
	}
        else {
            $perl .= $$end{perl};
	}
    }
    #print "primary:[$perl]\n";
    {perl => $perl}
}

## attributeref ::= 
##              primary "." identifier

# note: conversion from left-recursive to right-recursive
attributeref : "." identifier
{
    my $perl = "->{" . $item{identifier}->{perl} . "}";
    {perl => $perl}
}

## subscription ::= 
##              primary "[" expression_list "]"

# note: conversion to left-recursive
subscription : "[" <skip: $skip_inner> expression_list "]"
{
    #FIX: handle list
    my $perl = "[" . $item{expression_list}->{perl} . "]";
    {perl => $perl, type => 'subscription'}
}

## slicing ::= 
##              simple_slicing | extended_slicing

slicing : 
    simple_slicing
    #FIX | extended_slicing
{
    my $ret = $item[1];
    $$ret{perl} = "[" . $$ret{perl} . "]";
    $$ret{type} = 'slicing';
    $ret
}

## simple_slicing ::= 
##              primary "[" short_slice "]"
# note: conversion to right-recursive
simple_slicing : "[" short_slice "]"
{
    my $perl = $item{short_slice}->{lower}{perl} . ".."
        . $item{short_slice}->{upper}{perl};

    {perl => $perl}
}

#FIX:TODO
#note:<skip: $skip_inner>

## extended_slicing ::= 
##              primary "[" slice_list "]" 

# note: conversion to right-recursive
#FIX, wrong
extended_slicing : "[" <skip: $skip_inner> slice_list "]" 
{
    my $parts = $item[3];
    #FIX:what if range upper bound not defined?
    my $perl = join(', ', map {
        if(&def($$_{type} eq 'short_slice')) {
            $$_{lower}{perl} . ".." . $$_{upper}{perl}
	}
        else { die "FIX:UNIMPLEMENTED"; }
    } @$parts);
    #die $perl;
    {perl => $perl}
}

## slice_list ::= 
##              slice_item ("," slice_item)* [","]

slice_list : slice_item(s /,/) (",")(?)
{
    my $eles = $item[1];

    $eles;
}

## slice_item ::= 
##              expression | proper_slice | ellipsis

# note: expression after others
slice_item : (proper_slice #FIX: | ellipsis
              | expression
               )
{
    $item[1]
}


## proper_slice ::= 
##              short_slice | long_slice

proper_slice : short_slice
       # FIX: | long_slice
{
    $item[1]
}


## short_slice ::= 
##              [lower_bound] ":" [upper_bound]

short_slice : lower_bound(?) ":" upper_bound(?)
{
    my $lower = &optional($item{'lower_bound(?)'});
    $lower = {perl => "0"} if !defined($lower);
    my $upper = &optional($item{'upper_bound(?)'});
    #FIX:what to do about upper?
    {lower => $lower, upper => $upper, type=>'short_slice'}
}

## long_slice ::= 
##              short_slice ":" [stride]

#FIX:TODO

## lower_bound ::= 
##              expression

lower_bound : expression

## upper_bound ::= 
##              expression

upper_bound : expression

## stride ::= 
##              expression

stride : expression

## ellipsis ::= 
##              "..."

#FIX:TODO

## call ::= 
##              primary "(" [argument_list [","]] ")"

# note: conversion from left-recursive to right-recursive
call : "(" <skip: $skip_inner> ( argument_list (",")(?) {$item[1]} )(?) ")"
{
    my $arglist = &optional($item[3]);
    my $argperl = defined($arglist) ? $$arglist{perl} : '';
    my $perl = '('
        . $argperl . ')';
    {perl => $perl, type => 'call'}
}

## argument_list ::= 
##              positional_arguments ["," keyword_arguments]
##                                      ["," "*" expression]
##                                      ["," "**" expression]
##                 | keyword_arguments ["," "*" expression]
##                                     ["," "**" expression]
##                 | "*" expression ["," "**" expression]
##                 | "**" expression

# IMPROVE: is there a simpler way?
argument_list : (
    positional_arguments ("," keyword_arguments)(?)
                         ("," "*" expression)(?)
                         ("," "**" expression)(?)
    { {pos_args => $item[1],
       key_args => &optional($item[2]),
       morepos_args => &optional($item[3]),
       morekey_args => &optional($item[4])
    } }
    | keyword_arguments ("," "*" expression)(?)
                        ("," "**" expression)(?)
    { {key_args => $item[1],
       morepos_args => &optional($item[2]),
       morekey_args => &optional($item[3])
    } }
    | "*" expression ("," "**" expression)(?)
    { {morepos_args => $item[2],
       morekey_args => &optional($item[3])
    } }
    | "**" expression
    { {morekey_args => $item[2]
    } }
)
{
    my $parts = $item[1];

    # FIX: other argument types
    my $perl = defined($$parts{pos_args}) ? $$parts{pos_args}->{perl} : '';

    {perl => $perl}
}

## positional_arguments ::= 
##              expression ("," expression)*

positional_arguments : expression(s? /,/)
{
    my $perl = join(', ', map {$$_{perl}} @{$item[1]});

    {perl => $perl}
}

## positional_arguments ::= 
##              expression ("," expression)*
  
keyword_arguments : keyword_item(s? /,/)
{
    my $perl = join(', ', map {$$_{perl}} @{$item[1]});

    {perl => $perl}
}

## keyword_item ::= 
##              identifier "=" expression
  
keyword_item : identifier "=" expression
{
    my $perl = $item{identifier}->{perl}
        . " = " . $item{expression}->{perl};

    {perl => $perl}
}


## power ::= 
##              primary ["**" u_expr]

power : primary ("**" u_expr)(s?)
{
    my $uexprs = $item[2];

    my $perl = $item{primary}->{perl}
        . join('', map "**$$_{perl}", @$uexprs);
    {perl => $perl}
}


## u_expr ::= 
##              power | "-" u_expr
##               | "+" u_expr | "\~" u_expr

u_expr : ("-" | "+" | "~")(s?) power
{
    my $parts = $item[1];
    my $perl = join('', @$parts) . $item{power}->{perl};
    {perl => $perl}
}

## m_expr ::= 
##              u_expr | m_expr "*" u_expr
##               | m_expr "//" u_expr
##               | m_expr "/" u_expr
##                 | m_expr "\%" u_expr

m_expr : <leftop: u_expr ('*' | '//' | '/' | '%') u_expr>
{
    my $parts = $item[1];
    my $perl = join(' ', map {ref($_) ? "$$_{perl}" : $_}
        @$parts);

    {perl => $perl}
}

  

## a_expr ::= 
##             m_expr | a_expr "+" m_expr
##              | a_expr "-" m_expr

a_expr : <leftop: m_expr ('+' | '-') m_expr>
{
    my $parts = $item[1];
    my $perl = join(' ', map {ref($_) ? "$$_{perl}" : $_}
        @$parts);

    {perl => $perl}
}


## shift_expr ::= 
##             a_expr
##              | shift_expr ( "<<" | ">>" ) a_expr

shift_expr : a_expr
             #  | shift_expr ( "<<" | ">>" ) a_expr
{
    $item[1];
}


## and_expr ::= 
##             shift_expr | and_expr "\;SPMamp;" shift_expr

and_expr : shift_expr # | and_expr "\;SPMamp;" shift_expr
{
    $item[1]
}
  

## xor_expr ::= 
##             and_expr | xor_expr "\textasciicircum" and_expr

xor_expr : and_expr(s /\^/)
{
    my $ands = $item[1];
    my $perl = join(' ^ ', map "$$_{perl}", @$ands);
    {perl => $perl}
}  

## or_expr ::= 
##              xor_expr | or_expr "|" xor_expr

or_expr : xor_expr(s /\|/)
{
    my $xors = $item[1];
    my $perl = join(' | ', map "$$_{perl}", @$xors);

    {perl => $perl}
}


## comparison ::= 
##              or_expr ( comp_operator or_expr )*

comparison : 
             <leftop: or_expr comp_operator or_expr>
{
    my $parts = $item[1];
    my $perl = join(' ', map {ref($_) ? "$$_{perl}" : $_}
        @$parts);

    {perl => $perl}
}


## comp_operator ::= 
##              "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
##                 | "is" ["not"] | ["not"] "in"


comp_operator : 
             "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
                | "is" ("not")(?) #FIX
                | ("not")(?) "in" #FIX


## expression ::= 
##              or_test | lambda_form

expression : or_test
             # | lambda_form
{
    $item{'or_test'}
}


## or_test ::= 
##              and_test | or_test "or" and_test
  
or_test : 
             and_test(s /or/)
{
    my $ands = $item{'and_test(s)'};
    my $perl = join(' or ', map "$$_{perl}", @$ands);

    {perl => $perl}
}

## and_test ::= 
##              not_test | and_test "and" not_test

  
and_test : not_test(s /and/)
{
    my $nots = $item{'not_test(s)'};
    my $perl = join(' and ', map "$$_{perl}", @$nots);
    {perl => $perl}
}

## not_test ::= 
##              comparison | "not" not_test

not_test : ("not" <commit>)(s?) comparison
{
    my $nots = $item[1];
    my $perl = (join '', map "not ", @$nots)
        . $item{comparison}->{perl};
    {perl => $perl}
}
| <error?>

## lambda_form ::= 
##              "lambda" [parameter_list]: expression

#FIX:TODO

## expression_list ::= 
##              expression ( "," expression )* [","]

expression_list : expression(s /,/) (',')(?)
{
    my $expressions = $item{'expression(s)'};
    my $perl = @$expressions == 1 ? $$expressions[0]->{perl}
        : '(' . join(', ', map "$$_{perl}", @$expressions) . ')';

    {perl => $perl}
}

## simple_stmt ::= expression_stmt
##                 | assert_stmt
##                 | assignment_stmt
##                 | augmented_assignment_stmt
##                 | pass_stmt
##                 | del_stmt
##                 | print_stmt
##                 | return_stmt
##                 | yield_stmt
##                 | raise_stmt
##                 | break_stmt
##                 | continue_stmt
##                 | import_stmt
##                 | global_stmt
##                 | exec_stmt

#FIX: if expression_stmt succeed but higher-level rule fails,
# then it does not try assignment_stmt
#
#
simple_stmt : <rulevar: local $pass_commit = \$commit>
simple_stmt :
              assert_stmt
              | continue_stmt
              | break_stmt
              | return_stmt
              | pass_stmt
              # note: print_stmt before expression_stmt
              | print_stmt
#FIX:            | del_stmt
#                | yield_stmt
#                | raise_stmt
#                | import_stmt
#                | global_stmt
#                | exec_stmt
              | assignment_stmt
              | augmented_assignment_stmt
              # note: expression_stmt after assignment_stmt
              | expression_stmt
| <error?>


## expression_stmt ::= 
##              expression_list

expression_stmt : expression_list


## assert_stmt ::= 
##              "assert" expression ["," expression]

assert_stmt : "assert" expression ("," expression)(?)
{
    #FIX: handle __debug__ and the second expression
    my $expr1 = $item[2];

    my $perl = "die if not $$expr1{perl}";

    {perl => $perl}
}

## assignment_stmt ::= 
##              (target_list "=")+ expression_list

assignment_stmt : 
             (target_list "=" {$item[1]})(s) <commit>
             {$$pass_commit = 1}
              expression_list
{
    my $tlist = $item[1];
    my $perl = join('', map "$$_{perl} = ", @$tlist)
        . $item{expression_list}->{perl};
    #print STDERR "[assignment_stmt:$perl]";
    {perl => $perl}
}
| <error?>
  
## target_list ::= 
##              target ("," target)* [","]

target_list : 
             target(s /,/) (',')(?)
{
    my $targets = $item{'target(s)'};

    my $perl = @$targets == 1 ? "$$targets[0]{perl}"
        : '(' . join(', ', map "$$_{perl}", @$targets) . ')';
    #print STDERR "[target_list:$perl]";
    {perl => $perl}
}

## target ::= 
##              identifier
##                 | "(" target_list ")"
##                 | "[" target_list "]"
##                 | attributeref
##                 | subscription
##                 | slicing

# note: converted to right-recursive
target : atom (attributeref | subscription
    #FIX: | slicing
    #FIX: | "(" <skip: $skip_inner> target_list ")"
    #FIX: | "[" <skip: $skip_inner> target_list "]"
    )(s?)
{
    my $ends = $item[2];

    #print STDERR Dumper(\%item);
    my $atom = $item{atom}->{perl};
    
    if(&def($item{atom}->{type}) eq 'identifier') {
        $atom = "\$$atom"
    }
    elsif(@$ends > 0 && &def($$ends[0]->{type}) eq 'call') {
        $atom = "\&$atom";
    }

    my $perl = $atom . join('', map {$$_{perl}} @$ends);
    {perl => $perl}
}

## augmented_assignment_stmt ::= 
##              target augop expression_list

augmented_assignment_stmt :
             target augop <commit> {$$pass_commit = 1} expression_list
{
    my $perl = $item{target}->{perl} . " " . $item{augop}->{perl}
        . " " . $item{expression_list}->{perl};
    #print STDERR "[augmented_assignment_stmt:$perl]";
    {perl => $perl}
}
| <error?>

## augop ::= 
##              "+=" | "-=" | "*=" | "/=" | "\%=" | "**="
##                 | ">>=" | "<<=" | "\&=" | "\textasciicircum=" | "|="

augop : ("+=" | "-=" | "*=" | "/=" | '%=' | "**="
        | ">>=" | "<<=" | '&=' | "^=" | "|=")
{
    {perl => $item[1]}
}

## pass_stmt ::= 
##              "pass"

#FIX: this might not work correctly
pass_stmt : "pass"
{
    {perl => ''}
}

## del_stmt ::= 
##              "del" target_list

#FIX: TODO

## print_stmt ::= 
##              "print" ( \optionalexpression ("," expression)* \optional","
##                 | ">\code>" expression
##                   \optional("," expression)+ \optional"," )

expressions: expression(s /,/)
print_stmt :  "print" (
    ( expressions (",")(?) {$item[1]} )(?)
{
    $_ = $item[1];
    my $exprs = @$_ == 0 ? [] : $$_[0];

    my $perl = "print " . join(', ', map "$$_{perl}", @$exprs);

    {perl => $perl}
}
    #FIX: | ">>" expression [("," expression)+ [","]]
    )

## return_stmt ::= 
##              "return" [expression_list]

return_stmt : "return" expression_list(?)
{
    my $elist = &optional($item{'expression_list(?)'});
    my $perl = "return" . (defined($elist) ? " $$elist{perl}" : '');
    {perl => $perl}
}

## yield_stmt ::= 
##              "yield" expression_list

#FIX:TODO

## raise_stmt ::= 
##              "raise" [expression ["," expression
##               ["," expression]]]

#FIX:TODO

## break_stmt ::= 
##              "break"

break_stmt : "break"
{
    {perl => "last"}
}

## continue_stmt ::= 
##              "continue"

continue_stmt : "continue"
{
    {perl => "next"}
}

## import_stmt ::= 
##              "import" module ["as" name]
##                 ( "," module ["as" name] )*
##                 | "from" module "import" identifier
##                     ["as" name]
##                   ( "," identifier ["as" name] )*
##                 | "from" module "import" "*"

#FIX:TODO

## module ::= 
##              (identifier ".")* identifier

## global_stmt ::= 
##              "global" identifier ("," identifier)*

## exec_stmt ::= 
##              "exec" expression
##               ["in" expression ["," expression]]

## compound_stmt ::= 
##              if_stmt
##                 | while_stmt
##                 | for_stmt
##                 | try_stmt
##                 | funcdef
##                 | classdef

compound_stmt : 
             if_stmt
             | while_stmt
             | for_stmt
              #  | try_stmt
             | funcdef
             | classdef

## suite ::= 
##              stmt_list NEWLINE
##               | NEWLINE INDENT statement+ DEDENT

suite : stmt_list NEWLINE
{
    $item[1]
}
| NEWLINE INDENT statement(s) DEDENT
{
    my $stmts = $item{'statement(s)'};

    my $perl = join('', map "$$_{perl}", @$stmts);
    {perl => $perl}
}

## statement ::= 
##              stmt_list NEWLINE | compound_stmt

statement : (
    .../if\b|while\b|for\b|try\b|def\b|class\b/ <commit>  compound_stmt
    | stmt_list NEWLINE {$item[1]})
{
    my $stmt = $item[1];
    my $perl = $stmt->{perl};

    {perl => $perl}
}
| <error?>

## stmt_list ::= 
##              simple_stmt (";" simple_stmt)* [";"]

stmt_list : simple_stmt(s /;/) (';')(?) ...NEWLINE
{
    my $stmts = $item[1];
    my $perl = join('; ', map "$$_{perl}", @$stmts) . ";\n";
    {perl => $perl}
}

## if_stmt ::= 
##              "if" expression ":" suite
##                 ( "elif" expression ":" suite )*
##                 ["else" ":" suite]

if_stmt : "if" expression ":" suite
                ("elif" expression ":" suite)(s?)
                ("else" ":" suite)(?)
{
    my $elifs = $item[5];
    my $else = &optional($item[6]);
    my $perl = "if(" . $item{expression}->{perl} . ") {\n"
        . &indent($item{suite}->{perl}) . "}\n";
    foreach my $elif (@$elifs) {
        $perl .= "elsif(" . $item{expression}->{perl} . ") {\n"
        . &indent($$elif{perl}) . "}\n";
    }
    if($else) {
        $perl .= "else {\n"
        . &indent($$else{perl}) . "}\n";
    }
    {perl => $perl}
}

## while_stmt ::= 
##              "while" expression ":" suite
##                 ["else" ":" suite]

while_stmt : "while" <commit> expression ":" suite
              #FIX:  ("else" ":" suite)(?)
{
    #die Dumper(\%item);
    my $perl = "while(" . $item{expression}->{perl} . ") {\n"
        . &indent($item{suite}->{perl}) . "}\n";
    {perl => $perl}
}
| <error?>


## for_stmt ::= 
##              "for" target_list "in" expression_list
##               ":" suite
##                 ["else" ":" suite]

#FIX: handle arbitrary target_list
    for_stmt : "for" target_list "in" expression_list
           ":" suite 
           #FIX: ("else" ":" suite)(?)
{
    my $perl = "foreach my $item{target_list}->{perl} "
        . "$item{expression_list}->{perl} {\n"
        . &indent($item{suite}->{perl})
        . "}\n";
    {perl => $perl}
}

## try_stmt ::= 
##              try_exc_stmt | try_fin_stmt

#FIX: todo
  
## try_exc_stmt ::= 
##              "try" ":" suite
##                 ("except" [expression
##                              ["," target]] ":" suite)+
##                 ["else" ":" suite]

#FIX: todo
  
## try_fin_stmt ::= 
##              "try" ":" suite
##               "finally" ":" suite

#FIX: todo


## funcdef ::= 
##              "def" funcname "(" [parameter_list] ")"
##               ":" suite

funcdef : "def" funcname
          "(" <skip: $skip_inner> parameter_list(?) ")" <skip: $skip_outer>
           ":" suite
{
    my $pl = &optional($item{'parameter_list(?)'});
    my $funcname = $item{funcname}->{perl};

    my $perl;
    if($item{funcname}->{perl} eq '__init__') { # contructor
        die ["[EX] Missing self param in __init__ function.". $thisline]
            if !defined($pl) || @{$$pl{elements}} < 1;
        my @used_params = @{$$pl{elements}}[1..@{$$pl{elements}}-1];
        my $self_var = $$pl{elements}[0]{perl};

        $perl = "sub new {\n"
            . &indent(
                ((@used_params > 0) ?
                    ("my(" . join(", ", ('$class', map {$$_{perl}} @used_params)) . ") = \@_;\n")
                    : '')
                . "my $self_var = {};\n"
 	        . "bless $self_var, \$class;\n"
                . $item{suite}->{perl}
                . "return $self_var;\n"
            )
            . "}\n";
    }
    else { # normal function
        $perl = "sub $funcname {\n"
            . &indent(
                ((defined($pl) && $$pl{perl} ne '') ?
                    ("my(" . $$pl{perl} . ") = \@_;\n") : '')
                . $item{suite}->{perl}
            ) . "}\n";
    }

    {perl => $perl}
}
  

## parameter_list ::= 
##              (defparameter ",")*
##                 ("*" identifier [, "**" identifier]
##                 | "**" identifier
##                   | defparameter [","])

#note: error in above grammar: comma should be quoted

# ^ why so ugly?

#note: make this nullable
parameter_list : 
    defparameter(s /,/) (","
        ("*" identifier ("," "**" identifier)(?))(?)
    )(?)
{
    #FIX: * and ** params
    my $defparams = $item[1];

    my $perl = join(', ', map "$$_{perl}", @$defparams);

    {perl => $perl, elements => $defparams}
}


## defparameter ::= 
##              parameter ["=" expression]
  
defparameter : parameter #FIX: ("=" expression)(?)
{
    {perl => '$' . $item[1]->{perl}}
}

## sublist ::= 
##              parameter ("," parameter)* [","]

#FIX:TODO

## parameter ::= 
##              identifier | "(" sublist ")"

parameter : identifier #FIX: | "(" <skip: $skip_inner> sublist ")"
{
    {perl => $item[1]->{perl}}
}


## funcname ::= 
##              identifier

funcname : identifier


## classdef ::= 
##              "class" classname [inheritance] ":"
##               suite


classdef : "class" classname (inheritance)(?) ":"
           suite
{
    # FIX: inheritance
    my $perl = "{\n" . &indent(
        "package $item{classname}->{perl};\n" . $item{suite}->{perl}
    ) . "}\n";
    {perl => $perl}
}

## inheritance ::= 
##              "(" [expression_list] ")"
  
inheritance : "(" <skip: $skip_inner> expression_list(?) ")"

## classname ::= 
##              identifier

classname : identifier



file_input : <skip: $skip_outer>
             (NEWLINE | statement)(s?) end_of_file
{
    my $stmts = $item[2];

    my $perl = join('', map "$$_{perl}", grep {ref($_)} @$stmts);

    # disambiguate function calls and constructors
    # FIX: major hack
    $perl =~ s{__defer_call_([a-zA-Z_][a-zA-Z0-9_]*)}{
        my $name = $1;
        my $out;
        if($perl =~ /\bpackage\s+$name\b/) {
            #note: might also do eval("defined(&$name::new)") or something
            #  to detect Perl classes
            $out = "new $name";
	}
        else {
            $out = "\&$name";
	}
        $out;
    }gse;

    $perl;
}
| <error>


## interactive_input ::= 
##              [stmt_list] NEWLINE | compound_stmt NEWLINE

#FIX:todo

## eval_input ::= 
##              expression_list NEWLINE*

#FIX:todo

## input_input ::= 
##              expression_list NEWLINE

#FIX:todo
