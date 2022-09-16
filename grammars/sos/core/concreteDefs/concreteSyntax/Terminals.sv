grammar sos:core:concreteDefs:concreteSyntax;

imports sos:core:common:concreteSyntax;


terminal Ignore_t   'ignore';

terminal Integer_t   /-?[0-9]+/;
terminal String_t    /"([^"]|(\\"))*"/;

terminal ProdPart_t   /$[0-9]+/;
terminal ToInt_t      '$to_int';

terminal LParen_t   '(';
terminal RParen_t   ')';
terminal LAngle_t   '<';
terminal RAngle_t   '>';
terminal Comma_t    ',';
terminal Semi_t     ';';
terminal Colon_t    ':';
terminal Period_t   '.';

--Regex
terminal LBracket_t   '[';
terminal RBracket_t   ']';
terminal Star_t       '*';
terminal Plus_t       '+';
terminal Or_t         '|';
terminal Char_t       /./;
terminal Range_t      '-';

terminal ColonsEq_t   '::=';
terminal Produces_t   '~~>';

terminal StringTy_t   'string';
terminal IntTy_t      'int';

