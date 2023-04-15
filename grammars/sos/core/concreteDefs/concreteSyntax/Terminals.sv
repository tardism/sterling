grammar sos:core:concreteDefs:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:concreteDefs:abstractSyntax;


terminal Ignore_t   'ignore' lexer classes {KEYWORD};

terminal Integer_t   /-?[0-9]+/;
terminal String_t    /"([^"]|(\\"))*"/;

terminal ProdPart_t   /[A-Z][a-z0-9A-Z_]*/;
terminal ToInt_t      '$to_int';

terminal LAngle_t   '<';
terminal RAngle_t   '>';
terminal Comma_t    ',';
terminal Semi_t     ';';
terminal Colon_t    ':';
terminal Period_t   '.';
terminal Slash_t    '/';

terminal DoubleColon_t   '::'   association=right;

--Regex
lexer class REGEX_SYMBOL dominates {Char_t};
terminal LBracket_t   '[' lexer classes {REGEX_SYMBOL};
terminal RBracket_t   ']' lexer classes {REGEX_SYMBOL};
terminal Star_t       '*' lexer classes {REGEX_SYMBOL};
terminal Plus_t       '+' lexer classes {REGEX_SYMBOL};
terminal Or_t         '|' lexer classes {REGEX_SYMBOL};
terminal Char_t       /.|(\\[\ ntr\-\/\\abf+*|()\[\]])|(\\[0-9][0-9][0-9])/ submits to {KEYWORD, LParen_t, RParen_t, Slash_t, Spacing_t};
terminal Range_t      '-' lexer classes {REGEX_SYMBOL};

terminal ColonsEq_t   '::=';
terminal Produces_t   '~~>';

terminal StringTy_t   'string';
terminal IntTy_t      'int';

