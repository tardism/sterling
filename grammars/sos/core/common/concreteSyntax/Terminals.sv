grammar sos:core:common:concreteSyntax;

imports sos:core:common:abstractSyntax;


lexer class KEYWORD dominates {LowerId_t};


terminal Module_t       'Module'        lexer classes {KEYWORD};


terminal StringTy_t   'string'   lexer classes {KEYWORD};
terminal IntTy_t      'int'      lexer classes {KEYWORD};

terminal LBracket_t   '[';
terminal RBracket_t   ']';
terminal LParen_t     '(';
terminal RParen_t     ')';

terminal Comma_t      ',';


{-
  We roll single-line comments into our newlines because we need to
  know when lines end a lot of the time, which precludes making them
  ignore terminals.  However, since every line end uses Newline_t,
  this allows us to treat them as if they were ignored and only the
  actual newline were important.
-}
terminal Newline_t        /(\/\/[^\n]*)?\r?\n/   precedence=10;


terminal LowerId_t      /[a-z][a-z0-9A-Z_]*/;
terminal LowerQName_t   /([a-zA-Z_]+:)+[a-z][a-z0-9A-Z_]*/;
--using these for constr(args) permits pairs to use parentheses
terminal LowerIdParen_t      /[a-z][a-z0-9A-Z_]*\(/;
terminal LowerQNameParen_t   /([a-zA-Z_]+:)+[a-z][a-z0-9A-Z_]*\(/;

function dropNameParen
String ::= x::String
{
  return substring(0, length(x) - 1, x);
}


ignore terminal Spacing_t   /[\ \t\r]+/;
ignore terminal Comment_t   /\/\*(\/\*([^\*]|\*+[^\/\*])*\*+\/|[^\*]|\*+[^\/\*])*\*+\//;

